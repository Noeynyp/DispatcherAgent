from pyswip import Prolog
from pathlib import Path


class PrologEngine:
    def __init__(self):
        self.prolog = Prolog()
        self._load_knowledge_base()

    def _load_knowledge_base(self):
        # Load dispatcher.pl
        prolog_file = Path(__file__).parent / "dispatcher.pl"
        self.prolog.consult(str(prolog_file))

    # -----------------------------
    # FACT ASSERTION
    # -----------------------------

    def assert_unit(self, unit_id, service, location, status="available", notes=""):
        query = f"assertz(unit({unit_id}, {service}, {location}, {status}, '{notes}'))"
        list(self.prolog.query(query))

    def assert_incident(self, incident_id, incident_type, severity, location, reported_time):
        query = f"add_incident({incident_id}, {incident_type}, {severity}, {location}, {reported_time})"
        list(self.prolog.query(query))

    def assert_travel_time(self, unit_id, incident_id, seconds):
        # Remove old travel_time facts first
        list(self.prolog.query(f"retractall(travel_time({unit_id}, {incident_id}, _))"))
        query = f"assertz(travel_time({unit_id}, {incident_id}, {seconds}))"
        list(self.prolog.query(query))

    def assert_handling_time(self, incident_id, seconds):
        list(self.prolog.query(f"retractall(handling_time({incident_id}, _))"))
        query = f"assertz(handling_time({incident_id}, {seconds}))"
        list(self.prolog.query(query))

    # -----------------------------
    # QUERYING
    # -----------------------------

    def select_best_unit(self, incident_id):
        result = list(self.prolog.query(f"select_best_unit({incident_id}, U)"))
        if result:
            return result[0]["U"]
        return None

    def dispatch(self, incident_id):
        result = list(self.prolog.query(
            f"dispatch({incident_id}, Unit, Explanation)"
        ))
        if result:
            return result[0]["Unit"], result[0]["Explanation"]
        return None, "No unit available."

    def explain_assignment(self, incident_id, unit_id):
        result = list(self.prolog.query(
            f"explain_assignment({incident_id}, {unit_id}, Explanation)"
        ))
        if result:
            return result[0]["Explanation"]
        return None

    def escalate_urgency(self, incident_id, current_time):
        list(self.prolog.query(
            f"escalate_urgency({incident_id}, {current_time})"
        ))