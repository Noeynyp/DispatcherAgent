from src.simulation.simulation import (
    create_sample_graph,
    UnitState,
    advance_unit,
    compute_position,
    remaining_time_to_node,
    snap_to_nearest_node,
)

from src.prolog.prolog_engine import PrologEngine
import networkx as nx
import time


def main():
    print("=== Reassignment Validation Test ===")

    G = create_sample_graph()

    # Units start moving toward n3
    u1 = UnitState("u1", "fire", path=["n1", "n3"], speed=20.0)
    u2 = UnitState("u2", "fire", path=["n2", "n3"], speed=10.0)  # slower on purpose

    units = [u1, u2]

    engine = PrologEngine()

    engine.assert_unit("u1", "fire", "n1")
    engine.assert_unit("u2", "fire", "n2")

    # -----------------------------------------
    # First Incident at n3
    # -----------------------------------------
    incident1 = "i1"
    engine.assert_incident(incident1, "fire", 3, "n3", int(time.time()))

    for u in units:
        eta = remaining_time_to_node(u, "n3", G)
        engine.assert_travel_time(u.id, incident1, round(eta, 2))

    unit, _ = engine.dispatch(incident1)
    print("First dispatch:", unit)

    # -----------------------------------------
    # Simulate 4 seconds of movement
    # -----------------------------------------
    print("\nSimulating movement...")
    for i in range(4):
        for u in units:
            advance_unit(u, 1.0, G)
            pos = compute_position(u, G)
            print(f"t={i+1}s {u.id} at {pos}")

    # -----------------------------------------
    # New URGENT Incident at n4
    # -----------------------------------------
    print("\nURGENT incident at n4")

    incident2 = "i2"
    engine.assert_incident(incident2, "fire", 5, "n4", int(time.time()))

    # Recompute ETAs from CURRENT position
    for u in units:
        eta = remaining_time_to_node(u, "n4", G)
        engine.assert_travel_time(u.id, incident2, round(eta, 2))
        print(f"{u.id} NEW ETA → n4: {eta:.2f}s")

    # Ask Prolog again
    new_unit, explanation = engine.dispatch(incident2)

    print("\nReassignment Decision:")
    print("Selected:", new_unit)
    print("Explanation:", explanation)

    # -----------------------------------------
    # If reassigned, update path
    # -----------------------------------------
    selected_unit = next(u for u in units if u.id == new_unit)

    # Compute new shortest path
    current_position = compute_position(selected_unit, G)
    start_node = snap_to_nearest_node(current_position, G)

    new_path = nx.shortest_path(G, start_node, "n4", weight="weight")

    selected_unit.path = new_path
    selected_unit.edge_index = 0
    selected_unit.progress = 0.0

    print(f"\n{selected_unit.id} new path:", new_path)

    print("\n=== TEST COMPLETE ===")


if __name__ == "__main__":
    main()