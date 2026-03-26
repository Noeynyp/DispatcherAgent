from fastapi import FastAPI, Body
from fastapi.staticfiles import StaticFiles
from dataclasses import dataclass
import threading
import time
import networkx as nx

from src.simulation.simulation import (
    create_sample_graph,
    add_buildings,
    Building,
    UnitState,
    advance_unit,
    compute_position,
    remaining_time_to_node,
    snap_to_nearest_node
)

from src.prolog.prolog_engine import PrologEngine

app = FastAPI()

# Services required per incident type (mirrors required_service/2 in Prolog)
REQUIRED_SERVICES: dict[str, list] = {
    # Single-service
    "fire":              ["fire"],
    "medical":           ["medical"],
    "robbery":           ["police"],
    "cardiac":           ["medical"],
    "overdose":          ["medical"],
    "domestic":          ["police"],
    # Two-service
    "accident":          ["police", "medical"],
    "shooting":          ["police", "medical"],
    "structure_fire":    ["fire", "medical"],
    "hazmat":            ["fire", "medical"],
    "gas_leak":          ["fire", "police"],
    # Three-service
    "building_collapse": ["fire", "medical", "police"],
}

# Dispatcher's first-look estimate at incident creation — informational only.
ESTIMATED_HANDLING_TIMES: dict[str, dict[str, float]] = {
    "fire":              {"low": 30,  "medium": 60,  "high": 120, "critical": 240},
    "medical":           {"low": 20,  "medium": 45,  "high":  90, "critical": 180},
    "robbery":           {"low": 10,  "medium": 20,  "high":  35, "critical":  60},
    "cardiac":           {"low": 20,  "medium": 35,  "high":  60, "critical":  90},
    "overdose":          {"low": 15,  "medium": 28,  "high":  50, "critical":  80},
    "domestic":          {"low": 15,  "medium": 25,  "high":  45, "critical":  70},
    "accident":          {"low": 25,  "medium": 50,  "high": 100, "critical": 200},
    "shooting":          {"low": 25,  "medium": 50,  "high":  90, "critical": 150},
    "structure_fire":    {"low": 50,  "medium": 90,  "high": 160, "critical": 300},
    "hazmat":            {"low": 70,  "medium": 130, "high": 220, "critical": 400},
    "gas_leak":          {"low": 25,  "medium": 45,  "high":  90, "critical": 160},
    "building_collapse": {"low": 80,  "medium": 150, "high": 280, "critical": 500},
}

# On-scene handling time (seconds) by incident type → service → urgency.
# Set when a unit physically arrives and reports the real situation.
# Different services work different tasks — police clear scenes fast,
# fire teams may work for hours, medics vary by patient count/severity.
# Drives Prolog scoring: effective_response_time = handling_remaining + travel.
ON_SCENE_HANDLING_TIMES: dict[str, dict[str, dict[str, float]]] = {
    "fire": {
        "fire":    {"low": 20, "medium": 35, "high":  60, "critical":  90},
    },
    "medical": {
        "medical": {"low": 10, "medium": 20, "high":  35, "critical":  55},
    },
    "robbery": {
        "police":  {"low":  8, "medium": 14, "high":  22, "critical":  35},
    },
    "cardiac": {
        "medical": {"low": 15, "medium": 28, "high":  45, "critical":  70},
    },
    "overdose": {
        "medical": {"low": 10, "medium": 20, "high":  35, "critical":  55},
    },
    "domestic": {
        "police":  {"low": 10, "medium": 18, "high":  30, "critical":  50},
    },
    "accident": {
        "police":  {"low":  8, "medium": 12, "high":  18, "critical":  25},
        "medical": {"low": 12, "medium": 22, "high":  38, "critical":  60},
    },
    "shooting": {
        "police":  {"low": 12, "medium": 18, "high":  25, "critical":  40},
        "medical": {"low": 15, "medium": 28, "high":  50, "critical":  80},
    },
    "structure_fire": {
        "fire":    {"low": 30, "medium": 55, "high":  90, "critical": 150},
        "medical": {"low": 10, "medium": 18, "high":  30, "critical":  55},
    },
    "hazmat": {
        "fire":    {"low": 40, "medium": 70, "high": 120, "critical": 200},
        "medical": {"low":  8, "medium": 12, "high":  20, "critical":  35},
    },
    "gas_leak": {
        "fire":    {"low": 15, "medium": 28, "high":  50, "critical":  90},
        "police":  {"low":  8, "medium": 12, "high":  20, "critical":  30},
    },
    "building_collapse": {
        "fire":    {"low": 40, "medium": 80, "high": 140, "critical": 240},
        "medical": {"low": 20, "medium": 40, "high":  70, "critical": 120},
        "police":  {"low": 15, "medium": 28, "high":  50, "critical":  85},
    },
}


@dataclass
class ServiceSlot:
    """Tracks one service's lifecycle on an incident (e.g. police slot of an accident)."""
    service: str
    # Set at creation — dispatcher's estimate before any unit has seen the scene.
    estimated_handling_time: float = 0.0
    # Set when a unit arrives on scene — what the unit actually reports.
    on_scene_handling_time: float | None = None
    # Countdown while unit is handling. None until arrival.
    handling_remaining: float | None = None
    assigned_unit: str | None = None
    status: str = "waiting"  # waiting | assigned | in_progress | resolved


@dataclass
class IncidentState:
    id: str
    type: str
    severity: int
    location: str
    created_time: int
    urgency: str = "unknown"
    status: str = "waiting"   # waiting | assigned | in_progress | resolved
    # Per-service tracking — key is service name e.g. "fire", "police", "medical"
    service_slots: dict = None

    def __post_init__(self):
        if self.service_slots is None:
            self.service_slots = {}

    @property
    def assigned_units(self) -> list:
        """All units currently assigned or handling across all service slots."""
        return [s.assigned_unit for s in self.service_slots.values()
                if s.assigned_unit is not None]

# -----------------------------
# GLOBAL STATE
# -----------------------------

incidents: dict[str, IncidentState] = {}

G = create_sample_graph()
buildings = add_buildings(G)
buildings_by_id: dict[str, Building] = {b.id: b for b in buildings}

units = [
    UnitState("f1", "fire",    "n0_0"),
    UnitState("f2", "fire",    "n11_11"),
    UnitState("f3", "fire",    "n0_11"),
    UnitState("f4", "fire",    "n11_0"),
    UnitState("f5", "fire",    "n5_6"),
    UnitState("m1", "medical", "n3_3"),
    UnitState("m2", "medical", "n8_8"),
    UnitState("m3", "medical", "n3_8"),
    UnitState("m4", "medical", "n8_3"),
    UnitState("m5", "medical", "n6_1"),
    UnitState("p1", "police",  "n2_6"),
    UnitState("p2", "police",  "n9_5"),
    UnitState("p3", "police",  "n5_2"),
    UnitState("p4", "police",  "n5_9"),
    UnitState("p5", "police",  "n1_10"),
]


engine = PrologEngine()

for u in units:
    engine.assert_unit(u.id, u.service, u.current_node)


running = True


# -----------------------------
# 
# -----------------------------

def assign_unit_to_incident(unit_id: str, incident: IncidentState):

    unit = next(u for u in units if u.id == unit_id)

    # Clear old assignment
    engine.clear_assignment(unit_id)

    # Mark unit busy in Prolog
    engine.update_unit_status(
        unit_id,
        unit.service,
        unit.current_node,
        "busy"
    )

    # Assert assignment
    list(engine.prolog.query(
        f"assertz(assigned({unit_id}, {incident.id}))"
    ))

    slot = incident.service_slots.get(unit.service)
    if slot:
        slot.assigned_unit = unit_id
        slot.status = "assigned"
    incident.status = "assigned"

def handle_unit_arrival(unit_id: str):

    unit = next(u for u in units if u.id == unit_id)

    # Find assigned incident via Prolog (works for multi-service incidents
    # where Python's incident.assigned_unit only holds the last-assigned unit)
    assignment = list(engine.prolog.query(f"assigned({unit_id}, I)"))
    if not assignment:
        # No assignment found — free the unit and reevaluate so it can be dispatched
        unit.status = "idle"
        engine.update_unit_status(unit_id, unit.service, unit.current_node, "available")
        reevaluate_system()
        return

    incident_id = str(assignment[0]["I"])
    incident = incidents.get(incident_id)

    if not incident or incident.status == "resolved":
        unit.status = "idle"
        engine.update_unit_status(unit_id, unit.service, unit.current_node, "available")
        return

    # First unit to arrive transitions the incident to in_progress
    if incident.status == "assigned":
        incident.status = "in_progress"

    # Unit has seen the scene — set this service's on-scene handling time.
    # Uses type + urgency table (more specific than urgency-only).
    on_scene_duration = (ON_SCENE_HANDLING_TIMES
                         .get(incident.type, {})
                         .get(unit.service, {})
                         .get(incident.urgency, 10.0))

    slot = incident.service_slots.get(unit.service)
    if slot:
        slot.on_scene_handling_time = on_scene_duration
        slot.handling_remaining = on_scene_duration
        slot.status = "in_progress"

    unit.status = "handling"
    unit.handling_remaining = on_scene_duration

    # Use "handling" status in Prolog (distinct from "busy"=moving) so that
    # candidate_unit can include on-scene units in dispatch scoring.
    # handling_time is now per-unit so different services on the same incident
    # can have independent remaining times.
    engine.update_unit_status(unit_id, unit.service, unit.current_node, "handling")
    engine.assert_handling_time(unit_id, incident.id, on_scene_duration)

    print(f"[ARRIVED] {unit_id} → {incident.id} (status={incident.status})")

def handle_unit_finish(unit_id: str):

    unit = next(u for u in units if u.id == unit_id)

    # Look up incident via Prolog (same as handle_unit_arrival — avoids
    # relying on incident.assigned_unit which only holds the last-assigned unit)
    assignment = list(engine.prolog.query(f"assigned({unit_id}, I)"))

    # Free the unit regardless of what we find
    engine.clear_assignment(unit_id)
    engine.update_unit_status(unit_id, unit.service, unit.current_node, "available")
    unit.status = "idle"

    if assignment:
        incident_id = str(assignment[0]["I"])
        incident = incidents.get(incident_id)

        if incident and incident.status != "resolved":
            # Mark this unit's service slot as done
            slot = incident.service_slots.get(unit.service)
            if slot:
                slot.status = "resolved"
                slot.handling_remaining = 0.0
                slot.assigned_unit = None

            # Retract per-unit Prolog handling time
            list(engine.prolog.query(f"retractall(handling_time({unit_id}, {incident_id}, _))"))

            # Mark service as complete in Prolog — blocks re-dispatch to this
            # service even though the incident fact still exists for other services.
            list(engine.prolog.query(
                f"assertz(service_complete({incident_id}, {unit.service}))"
            ))

            # Check if every service slot is resolved
            all_done = all(s.status == "resolved" for s in incident.service_slots.values())
            if all_done:
                incident.status = "resolved"
                # Retract incident and all remaining Prolog facts
                list(engine.prolog.query(f"retractall(incident({incident_id}, _, _, _, _, _))"))
                list(engine.prolog.query(f"retractall(travel_time(_, {incident_id}, _))"))
                list(engine.prolog.query(f"retractall(handling_time(_, {incident_id}, _))"))
                list(engine.prolog.query(f"retractall(service_complete({incident_id}, _))"))
                print(f"[RESOLVED] {incident_id} — all services done")
            else:
                remaining = [s.service for s in incident.service_slots.values()
                             if s.status != "resolved"]
                print(f"[PARTIAL] {incident_id} — {unit.service} done, "
                      f"still waiting: {remaining}")

    # Consume any pre-assignment queued while this unit was handling
    pre_asgn = list(engine.prolog.query(f"pre_assigned({unit_id}, I)"))
    list(engine.prolog.query(f"retractall(pre_assigned({unit_id}, _))"))

    if pre_asgn:
        pending_id = str(pre_asgn[0]["I"])
        pending = incidents.get(pending_id)
        if pending and pending.status == "assigned" and unit_id in pending.assigned_units:
            print(f"[PRE-ASSIGN DISPATCH] {unit_id} → {pending_id}")
            assign_unit_to_incident(unit_id, pending)
            unit.path = nx.shortest_path(G, unit.current_node, pending.location, weight="weight")
            if len(unit.path) < 2:
                unit.current_node = pending.location
                unit.path = []
                unit.edge_index = 0
                unit.progress = 0.0
                handle_unit_arrival(unit.id)
            else:
                unit.edge_index = 0
                unit.progress = 0.0
                unit.status = "moving"
            reevaluate_system()
            return

    # No pre-assignment — reevaluate normally (idle takeover, fresh dispatch, etc.)
    reevaluate_system()


def maybe_escalate_incident(incident: IncidentState, current_time: int):
    """
    Escalate only if urgency actually changes.
    Trigger reevaluation only when escalation occurs.
    """

    # Query current urgency before escalation
    before = list(engine.prolog.query(
        f"incident({incident.id}, T, S, L, R, U)"
    ))

    if not before:
        return

    old_urgency = before[0]["U"]

    # Try escalation
    engine.escalate_urgency(incident.id, current_time)

    # Query urgency after escalation
    after = list(engine.prolog.query(
        f"incident({incident.id}, T, S, L, R, U)"
    ))

    new_urgency = after[0]["U"]

    if new_urgency != old_urgency:
        print(f"[ESCALATION] {incident.id}: {old_urgency} → {new_urgency}")

        incident.urgency = new_urgency

        # Update each slot's estimated_handling_time to reflect the new urgency
        type_table = ESTIMATED_HANDLING_TIMES.get(incident.type, {})
        new_est = type_table.get(new_urgency)
        if new_est is not None:
            for slot in incident.service_slots.values():
                slot.estimated_handling_time = new_est

        reevaluate_system()
        


def _refresh_travel_times():
    """
    Recompute Prolog travel times from current unit positions for all active incidents.
    Travel times are asserted at incident creation but units move — without refresh,
    scores use stale distances and dispatch/reassignment decisions are wrong.

    Handling units are now included as candidates (candidate_unit accepts 'handling').
    For them we also sync handling_time to the actual remaining seconds so that
    effective_response_time = remaining_handling + travel is accurate.
    """
    for unit in units:
        if unit.status == "handling":
            # Sync per-unit remaining handling time so Prolog scoring is current
            old_asgn = list(engine.prolog.query(f"assigned({unit.id}, I)"))
            if old_asgn:
                engine.assert_handling_time(
                    unit.id, str(old_asgn[0]["I"]), round(unit.handling_remaining, 2)
                )
        elif unit.status != "idle":
            continue  # skip moving units — they are not candidates

        for incident in incidents.values():
            if incident.status not in ("waiting", "assigned"):
                continue
            eta = remaining_time_to_node(unit, incident.location, G)
            engine.assert_travel_time(unit.id, incident.id, round(eta, 2))


def _check_reassignments() -> bool:
    """
    Check every moving unit against every waiting incident.
    Uses Prolog's better_reassignment/3, which now allows same-or-higher urgency
    so that a unit can cascade to an unattended incident at equal priority.
    Returns True if any reassignment was made so the caller can loop again.
    Only ONE reassignment is made per call — the loop in reevaluate_system retries.
    """
    for unit in units:
        if unit.status != "moving":
            continue

        old_assignment = list(engine.prolog.query(f"assigned({unit.id}, I)"))
        if not old_assignment:
            continue

        old_incident_id = str(old_assignment[0]["I"])
        old_incident = incidents.get(old_incident_id)
        if not old_incident or old_incident.status != "assigned":
            continue

        for new_incident in incidents.values():
            if new_incident.status != "waiting":
                continue

            reassess = list(engine.prolog.query(
                f"better_reassignment({unit.id}, {old_incident_id}, {new_incident.id})"
            ))

            if reassess:
                print(f"[REASSIGN] {unit.id}: {old_incident_id} → {new_incident.id}")

                old_incident.status = "waiting"
                for s in old_incident.service_slots.values():
                    s.assigned_unit = None
                    s.status = "waiting"
                engine.clear_assignment(unit.id)

                assign_unit_to_incident(unit.id, new_incident)

                # Preserve current edge progress to avoid visual warping.
                # Redirect from the next node in the current path onward.
                if unit.path and len(unit.path) >= 2 and unit.edge_index + 1 < len(unit.path):
                    next_node = unit.path[unit.edge_index + 1]
                    tail = nx.shortest_path(G, next_node, new_incident.location, weight="weight")
                    # Keep everything up to (not including) next_node, then append tail
                    unit.path = unit.path[:unit.edge_index + 1] + tail
                    # edge_index and progress are unchanged — unit continues on current edge
                else:
                    unit.path = nx.shortest_path(G, unit.current_node, new_incident.location, weight="weight")
                    if len(unit.path) < 2:
                        unit.current_node = new_incident.location
                        unit.path = []
                        unit.edge_index = 0
                        unit.progress = 0.0
                        handle_unit_arrival(unit.id)
                    else:
                        unit.edge_index = 0
                        unit.progress = 0.0

                return True  # Signal to reevaluate_system to loop again

    return False


def _stop_unit_mid_travel(u):
    """Snap unit to nearest edge endpoint and make it idle. Prevents warp-to-origin."""
    if u.path and u.edge_index + 1 < len(u.path):
        u.current_node = (u.path[u.edge_index + 1]
                          if u.progress >= 0.5 else u.path[u.edge_index])
    u.path = []
    u.edge_index = 0
    u.progress = 0.0
    u.status = "idle"
    engine.update_unit_status(u.id, u.service, u.current_node, "available")


def _check_idle_takeover() -> bool:
    """
    Three takeover scenarios — all return True on first match so reevaluate loops again.

    A) Moving unit takeover (status=assigned, unit is moving):
       Idle unit closer to the incident than the moving unit that's en route.

    B) In-progress moving unit takeover (status=in_progress, one service arrived but
       another service unit is still traveling):
       Idle unit closer to the incident for that specific service.

    C) Pre-assigned takeover:
       A handling unit has a queued next job (pre_assigned), but an idle unit
       could reach that job faster than handling_remaining + travel.
    """
    for idle_unit in units:
        if idle_unit.status != "idle":
            continue

        # ── A & B: moving unit en route to assigned or in_progress incident ──
        for incident in incidents.values():
            if incident.status not in ("assigned", "in_progress"):
                continue

            assigned_query = list(engine.prolog.query(f"assigned(U, {incident.id})"))
            for asgn in assigned_query:
                moving_unit_id = str(asgn["U"])
                moving_unit = next((u for u in units if u.id == moving_unit_id), None)
                if not moving_unit or moving_unit.status != "moving":
                    continue
                # Skip freshly dispatched (haven't moved yet)
                if moving_unit.edge_index == 0 and moving_unit.progress == 0.0:
                    continue
                # Idle unit must match the same service as the moving unit
                # (for in_progress: only steal the still-traveling service slot)
                if idle_unit.service != moving_unit.service:
                    continue

                idle_eta = remaining_time_to_node(idle_unit, incident.location, G)
                moving_eta = remaining_time_to_node(moving_unit, incident.location, G)

                if idle_eta < moving_eta:
                    print(f"[TAKEOVER-{'B' if incident.status == 'in_progress' else 'A'}] "
                          f"{idle_unit.id} ({idle_eta:.1f}s) faster than "
                          f"{moving_unit_id} ({moving_eta:.1f}s) → {incident.id}")

                    engine.clear_assignment(moving_unit_id)
                    _stop_unit_mid_travel(moving_unit)

                    # Only reset the specific service slot being stolen
                    slot = incident.service_slots.get(moving_unit.service)
                    if slot:
                        slot.assigned_unit = None
                        slot.status = "waiting"

                    # If no other units are still assigned, reset incident to waiting
                    still_assigned = any(
                        s.status in ("assigned", "in_progress")
                        for s in incident.service_slots.values()
                    )
                    if not still_assigned:
                        incident.status = "waiting"

                    return True

        # ── C: pre-assigned incident takeover ────────────────────────────────
        pre_query = list(engine.prolog.query(f"pre_assigned(H, I)"))
        for pa in pre_query:
            handling_unit_id = str(pa["H"])
            pending_id = str(pa["I"])

            handling_unit = next((u for u in units if u.id == handling_unit_id), None)
            if not handling_unit or handling_unit.status != "handling":
                continue
            if idle_unit.service != handling_unit.service:
                continue

            pending = incidents.get(pending_id)
            if not pending or pending.status not in ("assigned", "waiting"):
                continue

            idle_eta = remaining_time_to_node(idle_unit, pending.location, G)
            h_travel = remaining_time_to_node(handling_unit, pending.location, G)
            h_eta = (handling_unit.handling_remaining or 0.0) + h_travel

            if idle_eta < h_eta:
                print(f"[TAKEOVER-C] {idle_unit.id} ({idle_eta:.1f}s) faster than "
                      f"{handling_unit_id} (handling+travel={h_eta:.1f}s) → {pending_id}")

                list(engine.prolog.query(
                    f"retractall(pre_assigned({handling_unit_id}, {pending_id}))"
                ))
                slot = pending.service_slots.get(handling_unit.service)
                if slot:
                    slot.assigned_unit = None
                    slot.status = "waiting"

                # If no other units still assigned to pending, reset it to waiting
                still_assigned = any(
                    s.status in ("assigned", "in_progress")
                    for s in pending.service_slots.values()
                )
                if not still_assigned:
                    pending.status = "waiting"

                return True

    return False


def reevaluate_system():
    """
    Converging evaluation loop.

    Each iteration:
      1. Refresh travel times from current unit positions (fixes stale scoring).
      2. Try to dispatch one idle unit via Prolog's best_global_assignment.
         If dispatched → loop again (more units may be dispatchable).
      3. If no idle dispatch is possible, try to reassign one moving unit.
         If reassigned → loop again (freed incident may attract another unit).
      4. If neither dispatch nor reassignment is possible → system is stable, stop.

    A limit of 20 iterations prevents runaway loops on edge cases.
    """
    print("----- REEVALUATE -----")

    # Refresh once — physical positions don't change within a single reevaluate call,
    # so recomputing every iteration wastes O(units × incidents × 20) shortest-path calls.
    _refresh_travel_times()

    for _ in range(20):

        # ── Phase 0: idle unit takeover from moving unit ─────────────────────
        # A unit that just became idle may be closer to an already-assigned
        # incident than the unit currently traveling to it.
        if _check_idle_takeover():
            continue

        # ── Phase 1: reassign moving units ──────────────────────────────────
        # Run this BEFORE idle dispatch so that a moving unit closer to a new
        # high-priority incident can be redirected while that incident is still
        # "waiting". If we dispatched idle units first, the new incident would
        # be marked "assigned" before reassignment ever checked it.
        if _check_reassignments():
            continue  # Reassignment freed an incident → refresh and retry

        # ── Phase 2: dispatch idle units ────────────────────────────────────
        result = list(engine.prolog.query("best_global_assignment(I,S,U,Score)"))
        print("  best_global_assignment:", result)

        if result:
            incident_id = str(result[0]["I"])
            unit_id = str(result[0]["U"])
            incident = incidents.get(incident_id)
            unit = next(u for u in units if u.id == unit_id)

            # Stale resolved incident in Prolog — retract and retry
            if not incident or incident.status == "resolved":
                list(engine.prolog.query(f"retractall(incident({incident_id}, _, _, _, _, _))"))
                list(engine.prolog.query(f"retractall(travel_time(_, {incident_id}, _))"))
                list(engine.prolog.query(f"retractall(handling_time(_, {incident_id}, _))"))
                list(engine.prolog.query(f"retractall(service_complete({incident_id}, _))"))
                continue

            if unit.status == "idle":
                assign_unit_to_incident(unit_id, incident)
                # Idle units know their exact node (including building nodes) —
                # snap_to_nearest_node would skip building nodes and cause a warp.
                unit.path = nx.shortest_path(G, unit.current_node, incident.location, weight="weight")
                if len(unit.path) < 2:
                    # Unit is already at the destination — trigger immediate arrival
                    unit.current_node = incident.location
                    unit.path = []
                    unit.edge_index = 0
                    unit.progress = 0.0
                    handle_unit_arrival(unit.id)
                else:
                    unit.edge_index = 0
                    unit.progress = 0.0
                    unit.status = "moving"
                continue  # Loop: try to dispatch another idle unit

            if unit.status == "handling":
                # Unit is on-scene finishing another incident.
                # Pre-assign it: don't interrupt current handling, but queue
                # this incident so it dispatches the moment it finishes.
                print(f"[PRE-ASSIGN] {unit_id} → {incident_id} "
                      f"(will dispatch when handling finishes)")
                list(engine.prolog.query(f"retractall(pre_assigned({unit_id}, _))"))
                list(engine.prolog.query(f"assertz(pre_assigned({unit_id}, {incident_id}))"))
                slot = incident.service_slots.get(unit.service)
                if slot:
                    slot.assigned_unit = unit_id
                    slot.status = "assigned"
                incident.status = "assigned"
                continue  # Loop: another incident may still need a unit

        break  # No reassignment and no idle dispatch — fully stable


# -----------------------------
# SIMULATION LOOP
# -----------------------------

def simulation_loop():
    tick = 0.5
    while True:
        try:
            for u in units:
                event = advance_unit(u, tick, G)

                if event:
                    event_type, unit_id = event

                    if event_type == "arrived":
                        handle_unit_arrival(unit_id)

                    elif event_type == "finished_handling":
                        handle_unit_finish(unit_id)

            current_time = int(time.time())

            for incident in list(incidents.values()):
                if incident.status != "resolved":
                    maybe_escalate_incident(incident, current_time)

        except Exception as e:
            import traceback
            print(f"[SIMULATION ERROR] {e}")
            traceback.print_exc()

        time.sleep(tick)


threading.Thread(target=simulation_loop, daemon=True).start()


# -----------------------------
# API
# -----------------------------

@app.get("/graph")
def get_graph():
    nodes = []
    edges = []

    for node, data in G.nodes(data=True):
        nodes.append({
            "id": node,
            "x": data["pos"][0],
            "y": data["pos"][1],
            "type": data.get("type", "intersection")
        })

    # Only send road edges (intersection ↔ intersection)
    for u, v in G.edges():
        if G.nodes[u].get("type") == "intersection" and G.nodes[v].get("type") == "intersection":
            edges.append({"from": u, "to": v})

    # Compute map bounds from intersections so the frontend can set the viewBox
    ix = [d["pos"][0] for _, d in G.nodes(data=True) if d.get("type") == "intersection"]
    iy = [d["pos"][1] for _, d in G.nodes(data=True) if d.get("type") == "intersection"]
    margin = 40
    bounds = {
        "minX": min(ix) - margin,
        "minY": min(iy) - margin,
        "width":  max(ix) - min(ix) + 2 * margin,
        "height": max(iy) - min(iy) + 2 * margin,
    }

    return {"nodes": nodes, "edges": edges, "bounds": bounds}

@app.get("/state")
def get_state():
    unit_data = []
    for u in units:
        x, y = compute_position(u, G)
        pre = list(engine.prolog.query(f"pre_assigned({u.id}, I)"))
        unit_data.append({
            "id": u.id,
            "x": x,
            "y": y,
            "service": u.service,
            "status": u.status,
            "handling_remaining": round(u.handling_remaining, 1) if u.handling_remaining else None,
            "pre_assigned_to": str(pre[0]["I"]) if pre else None,
        })

    incident_data = []
    for i in incidents.values():
        incident_data.append({
            "id": i.id,
            "location": i.location,
            "type": i.type,
            "urgency": i.urgency,
            "status": i.status,
            "assigned_units": i.assigned_units,
            "service_slots": {
                svc: {
                    "status": s.status,
                    "assigned_unit": s.assigned_unit,
                    "estimated_handling_time": s.estimated_handling_time,
                    "on_scene_handling_time": s.on_scene_handling_time,
                    "handling_remaining": s.handling_remaining,
                }
                for svc, s in i.service_slots.items()
            },
        })

    return {"units": unit_data, "incidents": incident_data}


@app.post("/create_incident")
def create_incident(data: dict = Body(...)):

    location = data["location"]
    incident_type = data["type"]
    severity = data["severity"]

    incident_id = f"i{int(time.time()*1000)}"
    now = int(time.time())

    incident = IncidentState(
        id=incident_id,
        type=incident_type,
        severity=severity,
        location=location,
        created_time=now
    )
    incidents[incident_id] = incident

    engine.assert_incident(incident_id, incident_type, severity, location, now)

    # Read initial urgency that Prolog computed from severity
    urg_result = list(engine.prolog.query(f"incident({incident_id}, _, _, _, _, U)"))
    if urg_result:
        incident.urgency = str(urg_result[0]["U"])

    # Build per-service slots. Each slot gets the dispatcher's first-look estimate.
    # on_scene_handling_time is None until a unit physically arrives.
    type_table = ESTIMATED_HANDLING_TIMES.get(incident_type, {})
    est = type_table.get(incident.urgency, 30.0)
    for svc in REQUIRED_SERVICES.get(incident_type, []):
        incident.service_slots[svc] = ServiceSlot(
            service=svc,
            estimated_handling_time=est,
        )

    # Compute ETAs from current unit positions (building nodes are in the graph,
    # so shortest_path works correctly for both intersection and building locations)
    for u in units:
        eta = remaining_time_to_node(u, location, G)
        engine.assert_travel_time(u.id, incident_id, round(eta, 2))

    reevaluate_system()

    return {"message": "Incident registered. System reevaluated."}


# Serve frontend
app.mount("/", StaticFiles(directory="src/app/static", html=True), name="static")