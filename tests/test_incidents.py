import sys
import types
import threading

from fastapi.testclient import TestClient

# Prevent the simulation background thread from starting during import
_orig_thread = threading.Thread

class _DummyThread:
    def __init__(self, *args, **kwargs):
        pass
    def start(self):
        return None

threading.Thread = _DummyThread

# Provide a lightweight fake for pyswip so Prolog import succeeds
_pyswip = types.ModuleType("pyswip")
class _FakeProlog:
    def consult(self, *a, **k):
        return None
    def query(self, *a, **k):
        return []
_pyswip.Prolog = _FakeProlog
sys.modules["pyswip"] = _pyswip

# Import the app after mocks
from app import main as app_main

# Restore threading
threading.Thread = _orig_thread

client = TestClient(app_main.app)


def setup_function() -> None:
    # Reset in-memory store between tests
    app_main.incidents.clear()


class _FakeEngine:
    def __init__(self):
        self.prolog = types.SimpleNamespace(query=lambda *a, **k: [])
    def assert_incident(self, *a, **k):
        return None
    def assert_travel_time(self, *a, **k):
        return None
    def dispatch(self, incident_id):
        # return a deterministic assignment for tests
        return [("fire", "f1")], "dispatched for test"
    def assert_handling_time(self, *a, **k):
        return None
    def clear_assignment(self, *a, **k):
        return None
    def update_unit_status(self, *a, **k):
        return None


def test_create_incident():
    # Patch the engine and selected value to avoid source-side dependencies
    app_main.engine = _FakeEngine()
    app_main.selected = ["f1"]

    payload = {"node": "n0_0", "type": "fire", "severity": 3}

    response = client.post("/create_incident", json=payload)
    assert response.status_code == 200

    data = response.json()
    assert data["assigned"] == app_main.selected
    assert data["explanation"] == "dispatched for test"


def test_incident_recorded():
    # Ensure incidents dict contains the newly created incident
    setup_function()
    app_main.engine = _FakeEngine()
    app_main.selected = ["f1"]

    payload = {"node": "n0_0", "type": "medical", "severity": 2}
    resp = client.post("/create_incident", json=payload)
    assert resp.status_code == 200

    # There should be one incident recorded in memory
    assert len(app_main.incidents) == 1
    inc = list(app_main.incidents.values())[0]
    assert inc.type == "medical"
    assert inc.status == "assigned"

