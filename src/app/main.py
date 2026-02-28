from fastapi import FastAPI, Body
from fastapi.staticfiles import StaticFiles
from dataclasses import dataclass
import threading
import time
import networkx as nx

from src.simulation.simulation import (
    create_sample_graph,
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

    for incident in incidents.values():
        if incident.assigned_unit == unit_id and incident.status == "assigned":

            incident.status = "in_progress"

            handling_duration = 10

            unit.status = "handling"
            unit.handling_remaining = handling_duration

            engine.assert_handling_time(incident.id, handling_duration)

            print(f"[ARRIVED] {incident.id} → handling started")
            break

def handle_unit_finish(unit_id: str):

    unit = next(u for u in units if u.id == unit_id)

    for incident in incidents.values():
        if incident.assigned_unit == unit_id and incident.status == "in_progress":

            incident.status = "resolved"

            # Clear assignment
            engine.clear_assignment(unit_id)

            # Remove handling time
            list(engine.prolog.query(
                f"retractall(handling_time({incident.id}, _))"
            ))

            # Mark unit available again
            engine.update_unit_status(
                unit_id,
                unit.service,
                unit.current_node,
                "available"
            )

            unit.status = "idle"

            print(f"[RESOLVED] {incident.id} → {unit_id} now available")

            reevaluate_system()
            break


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
        


def reevaluate_system():

    while True:

        result = list(engine.prolog.query(
            "best_global_assignment(I,S,U,Score)"
        ))

        if not result:
            break

        incident_id = str(result[0]["I"])
        unit_id = str(result[0]["U"])

        incident = incidents.get(incident_id)
        unit = next(u for u in units if u.id == unit_id)

        if not incident:
            break

        # ----------------------------------------
        # CASE 1: UNIT IS AVAILABLE → NORMAL ASSIGN
        # ----------------------------------------
        if unit.status == "idle":

            assign_unit_to_incident(unit_id, incident)

        # ----------------------------------------
        # CASE 2: UNIT IS MOVING → CHECK REASSIGNMENT
        # ----------------------------------------
        elif unit.status == "moving":

            # Find old incident
            old_incident = None
            for i in incidents.values():
                if i.assigned_unit == unit_id and i.status in ("assigned", "waiting"):
                    old_incident = i
                    break

            if not old_incident:
                break

            # Ask Prolog if reassignment is better
            reassess = list(engine.prolog.query(
                f"better_reassignment({unit_id}, {old_incident.id}, {incident_id})"
            ))

            if reassess:

                print(f"[REASSIGN] {unit_id} from {old_incident.id} → {incident_id}")

                # Cancel old assignment
                old_incident.assigned_unit = None
                old_incident.status = "waiting"

                # Assign new one
                assign_unit_to_incident(unit_id, incident)

            else:
                break

        else:
            break

        # ----------------------------------------
        # Update movement path
        # ----------------------------------------
        current_pos = compute_position(unit, G)
        start_node = snap_to_nearest_node(current_pos, G)

        new_path = nx.shortest_path(
            G, start_node, incident.location, weight="weight"
        )

        unit.path = new_path
        unit.edge_index = 0
        unit.progress = 0.0
        unit.status = "moving"



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
            "y": data["pos"][1]
        })

    for u, v, data in G.edges(data=True):
        edges.append({
            "from": u,
            "to": v
        })

    return {
        "nodes": nodes,
        "edges": edges
    }

@app.get("/state")
def get_state():
    data = []
    for u in units:
        x, y = compute_position(u, G)
        data.append({
            "id": u.id,
            "x": x,
            "y": y,
            "service": u.service
        })
    return {"units": data}


@app.post("/create_incident")
def create_incident(data: dict = Body(...)):

    node = data["node"]
    incident_type = data["type"]
    severity = data["severity"]

    incident_id = f"i{int(time.time()*1000)}"
    now = int(time.time())

    incident = IncidentState(
        id=incident_id,
        type=incident_type,
        severity=severity,
        location=node,
        created_time=now
    )
    incidents[incident_id] = incident

    engine.assert_incident(incident_id, incident_type, severity, node, now)

    # Compute ETAs
    for u in units:
        eta = remaining_time_to_node(u, node, G)
        engine.assert_travel_time(u.id, incident_id, round(eta, 2))

    # Trigger global dispatch instead of per-incident dispatch
    reevaluate_system()

    return {
        "message": "Incident registered. System reevaluated."
    }


# Serve frontend
app.mount("/", StaticFiles(directory="src/app/static", html=True), name="static")