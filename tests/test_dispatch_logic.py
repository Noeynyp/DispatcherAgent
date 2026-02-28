from src.prolog.prolog_engine import PrologEngine

def reset_kb(engine):
    list(engine.prolog.query("retractall(incident(_,_,_,_,_,_))"))
    list(engine.prolog.query("retractall(unit(_,_,_,_,_))"))
    list(engine.prolog.query("retractall(travel_time(_,_,_))"))
    list(engine.prolog.query("retractall(assigned(_,_))"))
    list(engine.prolog.query("retractall(handling_time(_, _))"))

def print_scores(engine, incident_id):
    results = list(engine.prolog.query(
        f"""
        unit(U, Service, _, available, _),
        weighted_score({incident_id}, U, Score)
        """
    ))

    print("---- SCORES ----")
    for r in results:
        print(r["U"], "->", r["Score"])
    print("----------------")

def test_same_urgency_closer_unit():
    print("\nTEST 1: Same urgency, closer unit should win")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("m1", "medical", "n1")
    engine.assert_unit("m2", "medical", "n2")

    engine.assert_incident("i1", "medical", 3, "nX", 0)

    # m1 is far, m2 is close
    engine.assert_travel_time("m1", "i1", 40)
    engine.assert_travel_time("m2", "i1", 10)

    print_scores(engine, "i1")

    assignments, explanation = engine.dispatch("i1")
    print("Selected:", assignments)
    print("Explanation:", explanation)

def test_higher_urgency_over_distance():
    print("\nTEST 2: Higher urgency may beat shorter distance")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("f1", "fire", "n1")
    engine.assert_unit("f2", "fire", "n2")

    # Critical incident
    engine.assert_incident("i2", "fire", 5, "nX", 0)

    # f1 closer but f2 slightly farther
    engine.assert_travel_time("f1", "i2", 30)
    engine.assert_travel_time("f2", "i2", 35)

    print_scores(engine, "i2")

    assignments, explanation = engine.dispatch("i2")
    print("Selected:", assignments)
    print("Explanation:", explanation)

def test_accident_multi_service():
    print("\nTEST 3: Accident should dispatch police + medical")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("m1", "medical", "n1")
    engine.assert_unit("p1", "police", "n2")

    engine.assert_incident("i3", "accident", 4, "nX", 0)

    engine.assert_travel_time("m1", "i3", 15)
    engine.assert_travel_time("p1", "i3", 20)

    assignments, explanation = engine.dispatch("i3")

    print("Selected:", assignments)
    print("Explanation:", explanation)

def test_busy_unit_penalty():
    print("\nTEST 4: Busy unit should lose to available")

    engine = PrologEngine()
    reset_kb(engine)

    engine.assert_unit("m1", "medical", "n1")
    engine.assert_unit("m2", "medical", "n2")

    engine.assert_incident("i4", "medical", 3, "nX", 0)

    engine.assert_travel_time("m1", "i4", 5)
    engine.assert_travel_time("m2", "i4", 15)

    # m1 is busy with 30 seconds remaining
    list(engine.prolog.query("assertz(assigned(m1, old_incident))"))
    list(engine.prolog.query("assertz(handling_time(old_incident, 30))"))

    print_scores(engine, "i4")

    assignments, explanation = engine.dispatch("i4")
    print("Selected:", assignments)
    print("Explanation:", explanation)

if __name__ == "__main__":
    test_same_urgency_closer_unit()
    test_higher_urgency_over_distance()
    test_accident_multi_service()
    test_busy_unit_penalty()