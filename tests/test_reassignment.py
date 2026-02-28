from src.prolog.prolog_engine import PrologEngine


def reset_kb(engine):
    list(engine.prolog.query("retractall(incident(_,_,_,_,_,_))"))
    list(engine.prolog.query("retractall(unit(_,_,_,_,_))"))
    list(engine.prolog.query("retractall(travel_time(_,_,_))"))
    list(engine.prolog.query("retractall(assigned(_,_))"))
    list(engine.prolog.query("retractall(handling_time(_, _))"))


def test_reassign_on_higher_urgency():
    print("\nTEST 1: Reassign when higher urgency appears")

    engine = PrologEngine()
    reset_kb(engine)

    # One medical unit
    engine.assert_unit("m1", "medical", "n1")

    # Medium incident first
    engine.assert_incident("i_medium", "medical", 3, "loc", 0)
    engine.assert_travel_time("m1", "i_medium", 5)

    # Assign medium
    list(engine.prolog.query("assertz(assigned(m1, i_medium))"))

    # Now critical incident appears
    engine.assert_incident("i_critical", "medical", 5, "loc", 0)
    engine.assert_travel_time("m1", "i_critical", 25)

    # Check reassignment logic
    result = list(engine.prolog.query(
        "better_reassignment(m1, i_medium, i_critical)"
    ))

    print("Reassignment allowed?", bool(result))


def test_no_reassign_same_urgency():
    print("\nTEST 2: No reassignment if urgency same")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("m1", "medical", "n1")

    engine.assert_incident("i1", "medical", 3, "loc", 0)
    engine.assert_travel_time("m1", "i1", 5)
    list(engine.prolog.query("assertz(assigned(m1, i1))"))

    engine.assert_incident("i2", "medical", 3, "loc", 0)
    engine.assert_travel_time("m1", "i2", 3)

    result = list(engine.prolog.query(
        "better_reassignment(m1, i1, i2)"
    ))

    print("Reassignment allowed?", bool(result))


def test_no_reassign_if_score_worse():
    print("\nTEST 3: No reassignment if score worse")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("m1", "medical", "n1")

    # Medium first (close)
    engine.assert_incident("i1", "medical", 3, "loc", 0)
    engine.assert_travel_time("m1", "i1", 5)
    list(engine.prolog.query("assertz(assigned(m1, i1))"))

    # Critical but extremely far
    engine.assert_incident("i2", "medical", 5, "loc", 0)
    engine.assert_travel_time("m1", "i2", 200)

    result = list(engine.prolog.query(
        "better_reassignment(m1, i1, i2)"
    ))

    print("Reassignment allowed?", bool(result))


def test_no_reassign_when_handling():
    print("\nTEST 4: No reassignment while handling")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("m1", "medical", "n1")

    engine.assert_incident("i1", "medical", 3, "loc", 0)
    engine.assert_travel_time("m1", "i1", 5)

    # Assigned and handling
    list(engine.prolog.query("assertz(assigned(m1, i1))"))
    list(engine.prolog.query("assertz(handling_time(i1, 30))"))

    engine.assert_incident("i2", "medical", 5, "loc", 0)
    engine.assert_travel_time("m1", "i2", 10)

    result = list(engine.prolog.query(
        "better_reassignment(m1, i1, i2)"
    ))

    print("Reassignment allowed?", bool(result))


if __name__ == "__main__":
    test_reassign_on_higher_urgency()
    test_no_reassign_same_urgency()
    test_no_reassign_if_score_worse()
    test_no_reassign_when_handling()