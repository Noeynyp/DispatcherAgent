from src.prolog.prolog_engine import PrologEngine


def reset_kb(engine):
    list(engine.prolog.query("retractall(incident(_,_,_,_,_,_))"))
    list(engine.prolog.query("retractall(unit(_,_,_,_,_))"))
    list(engine.prolog.query("retractall(travel_time(_,_,_))"))
    list(engine.prolog.query("retractall(assigned(_,_))"))
    list(engine.prolog.query("retractall(handling_time(_, _))"))


def print_global_scores(engine):
    results = list(engine.prolog.query(
        """
        select_best_global_assignment(I, S, U, Score)
        """
    ))

    print("---- ALL POSSIBLE GLOBAL SCORES ----")
    for r in results:
        print(f"Incident {r['I']} | Service {r['S']} | Unit {r['U']} | Score {r['Score']}")
    print("------------------------------------")


def test_priority_over_distance():
    print("\nTEST 1: Critical should beat medium even if farther")

    engine = PrologEngine()
    reset_kb(engine)

    # Units
    engine.assert_unit("m1", "medical", "n1")
    engine.assert_unit("m2", "medical", "n2")

    # Incidents
    engine.assert_incident("i_medium", "medical", 3, "loc", 0)   # medium
    engine.assert_incident("i_critical", "medical", 5, "loc", 0) # critical

    # Travel times
    engine.assert_travel_time("m1", "i_medium", 5)
    engine.assert_travel_time("m2", "i_medium", 10)

    engine.assert_travel_time("m1", "i_critical", 25)
    engine.assert_travel_time("m2", "i_critical", 30)

    print_global_scores(engine)

    best = list(engine.prolog.query(
        "best_global_assignment(I,S,U,Score)"
    ))

    print("BEST GLOBAL:", best)


def test_two_incidents_one_unit():
    print("\nTEST 2: One unit, two incidents → highest score wins")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("f1", "fire", "n1")

    engine.assert_incident("i_low", "fire", 1, "loc", 0)      # low
    engine.assert_incident("i_high", "fire", 4, "loc", 0)     # high

    engine.assert_travel_time("f1", "i_low", 5)
    engine.assert_travel_time("f1", "i_high", 20)

    print_global_scores(engine)

    best = list(engine.prolog.query(
        "best_global_assignment(I,S,U,Score)"
    ))

    print("BEST GLOBAL:", best)


def test_multi_service_competition():
    print("\nTEST 3: Accident + Medical compete")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("m1", "medical", "n1")
    engine.assert_unit("p1", "police", "n2")

    engine.assert_incident("i_acc", "accident", 4, "loc", 0)
    engine.assert_incident("i_med", "medical", 3, "loc", 0)

    engine.assert_travel_time("m1", "i_acc", 15)
    engine.assert_travel_time("p1", "i_acc", 10)

    engine.assert_travel_time("m1", "i_med", 5)

    print_global_scores(engine)

    best = list(engine.prolog.query(
        "best_global_assignment(I,S,U,Score)"
    ))

    print("BEST GLOBAL:", best)


if __name__ == "__main__":
    test_priority_over_distance()
    test_two_incidents_one_unit()
    test_multi_service_competition()