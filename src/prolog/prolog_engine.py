from pyswip import Prolog
from pathlib import Path


class PrologEngine:
    def __init__(self):
        self.prolog = Prolog()
        self._load_knowledge_base()

    def _load_knowledge_base(self):
        prolog_file = Path(__file__).parent / "dispatcher.pl"
        self.prolog.consult(str(prolog_file))

    # -----------------------------
    # FACT ASSERTION
    # -----------------------------

    def assert_unit(self, unit_id, service, location, status="available", notes=""):
        list(self.prolog.query(
            f"assertz(unit({unit_id}, {service}, {location}, {status}, '{notes}'))"
        ))

    def update_unit_status(self, unit_id, service, location, status):
        # Remove all previous unit facts for this unit
        list(self.prolog.query(
            f"retractall(unit({unit_id}, _, _, _, _))"
        ))

        list(self.prolog.query(
            f"assertz(unit({unit_id}, {service}, {location}, {status}, ''))"
        ))

    def assert_incident(self, incident_id, incident_type, severity, location, reported_time):
        list(self.prolog.query(
            f"add_incident({incident_id}, {incident_type}, {severity}, {location}, {reported_time})"
        ))

    def assert_travel_time(self, unit_id, incident_id, seconds):
        list(self.prolog.query(
            f"retractall(travel_time({unit_id}, {incident_id}, _))"
        ))
        list(self.prolog.query(
            f"assertz(travel_time({unit_id}, {incident_id}, {seconds}))"
        ))

    def assert_handling_time(self, incident_id, seconds):
        list(self.prolog.query(
            f"retractall(handling_time({incident_id}, _))"
        ))
        list(self.prolog.query(
            f"assertz(handling_time({incident_id}, {seconds}))"
        ))

    def clear_assignment(self, unit_id):
        list(self.prolog.query(
            f"retractall(assigned({unit_id}, _))"
        ))

    # -----------------------------
    # QUERYING
    # -----------------------------

    def dispatch(self, incident_id):
        result = list(self.prolog.query(
            f"dispatch({incident_id}, Assignments, Explanation)"
        ))

        print("PROLOG RESULT:", result)

        if not result:
            return [], "No unit available."

        assignments_raw = result[0]["Assignments"]
        explanation = result[0]["Explanation"]

        parsed = []

        for item in assignments_raw:
            # item looks like "-(fire, f1)"
            text = str(item).strip()

            # Remove "-(" and ")"
            text = text.replace("-(", "").replace(")", "")

            service, unit = [x.strip() for x in text.split(",")]

            parsed.append((service, unit))

        return parsed, explanation

    def escalate_urgency(self, incident_id, current_time):
        list(self.prolog.query(
            f"escalate_urgency({incident_id}, {current_time})"
        ))