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

@dataclass
class IncidentState:
    id: str
    type: str
    severity: int
    location: str
    created_time: int
    urgency: str = "unknown"
    assigned_unit: str | None = None
    status: str = "waiting"   # waiting, assigned, resolved

# -----------------------------
# GLOBAL STATE
# -----------------------------

incidents: dict[str, IncidentState] = {}

G = create_sample_graph()
buildings = add_buildings(G, buildings_per_side=1)
buildings_by_id: dict[str, Building] = {b.id: b for b in buildings}

units = [
    UnitState("f1", "fire", "n0_0"),
    UnitState("f2", "fire", "n4_4"),
    UnitState("m1", "medical", "n0_4"),
    UnitState("m2", "medical", "n4_0"),
    UnitState("p1", "police", "n2_2"),
    UnitState("p2", "police", "n1_3"),
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

    incident.assigned_unit = unit_id
    incident.status = "assigned"

def handle_unit_arrival(unit_id: str):

    unit = next(u for u in units if u.id == unit_id)

    # Sync Prolog with the unit's actual current location
    engine.update_unit_status(unit_id, unit.service, unit.current_node, "busy")

    # Find assigned incident via Prolog (works for multi-service incidents
    # where Python's incident.assigned_unit only holds the last-assigned unit)
    assignment = list(engine.prolog.query(f"assigned({unit_id}, I)"))
    if not assignment:
        # No assignment found — free the unit
        unit.status = "idle"
        engine.update_unit_status(unit_id, unit.service, unit.current_node, "available")
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

    handling_duration = 10
    unit.status = "handling"
    unit.handling_remaining = handling_duration

    engine.assert_handling_time(incident.id, handling_duration)

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

    if not assignment:
        return

    incident_id = str(assignment[0]["I"])
    incident = incidents.get(incident_id)

    if not incident or incident.status == "resolved":
        return

    incident.status = "resolved"

    # Retract the incident and its data from Prolog so it can never
    # re-enter best_global_assignment and block new incidents
    list(engine.prolog.query(f"retractall(incident({incident_id}, _, _, _, _, _))"))
    list(engine.prolog.query(f"retractall(travel_time(_, {incident_id}, _))"))
    list(engine.prolog.query(f"retractall(handling_time({incident_id}, _))"))

    print(f"[RESOLVED] {incident_id} → {unit_id} now available")

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

        # 🔥 Trigger reevaluation ONLY NOW
        reevaluate_system()
        


def _refresh_travel_times():
    """
    Recompute Prolog travel times from current unit positions for all active incidents.
    Travel times are asserted at incident creation but units move — without refresh,
    scores use stale distances and dispatch/reassignment decisions are wrong.
    Handling units are skipped since they are on-scene and not candidates.
    """
    for unit in units:
        if unit.status == "handling":
            continue
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
                old_incident.assigned_unit = None
                engine.clear_assignment(unit.id)

                assign_unit_to_incident(unit.id, new_incident)

                # Preserve current edge progress to avoid visual warping.
                # Redirect from the next node in the current path onward.
                if unit.path and len(unit.path) >= 2:
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

    for _ in range(20):

        _refresh_travel_times()

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
                list(engine.prolog.query(f"retractall(handling_time({incident_id}, _))"))
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

        break  # No reassignment and no idle dispatch — fully stable


# -----------------------------
# SIMULATION LOOP
# -----------------------------

def simulation_loop():
    tick = 0.5
    while True:
        for u in units:
            event = advance_unit(u, tick, G)

            if event:
                event_type, unit_id = event

                if event_type == "arrived":
                    handle_unit_arrival(unit_id)

                elif event_type == "finished_handling":
                    handle_unit_finish(unit_id)

        current_time = int(time.time())

        for incident in incidents.values():
            if incident.status != "resolved":
                maybe_escalate_incident(incident, current_time)

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

    # Only send road edges (intersection ↔ intersection) — building connector
    # edges are implicit from building positions and don't need to be drawn.
    for u, v in G.edges():
        if G.nodes[u].get("type") == "intersection" and G.nodes[v].get("type") == "intersection":
            edges.append({"from": u, "to": v})

    return {"nodes": nodes, "edges": edges}

@app.get("/state")
def get_state():
    unit_data = []
    for u in units:
        x, y = compute_position(u, G)
        unit_data.append({
            "id": u.id,
            "x": x,
            "y": y,
            "service": u.service,
            "status": u.status
        })

    incident_data = []
    for i in incidents.values():
        incident_data.append({
            "id": i.id,
            "location": i.location,
            "type": i.type,
            "urgency": i.urgency,
            "status": i.status,
            "assigned_unit": i.assigned_unit
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

    # Compute ETAs from current unit positions (building nodes are in the graph,
    # so shortest_path works correctly for both intersection and building locations)
    for u in units:
        eta = remaining_time_to_node(u, location, G)
        engine.assert_travel_time(u.id, incident_id, round(eta, 2))

    reevaluate_system()

    return {"message": "Incident registered. System reevaluated."}


# Serve frontend
app.mount("/", StaticFiles(directory="src/app/static", html=True), name="static")