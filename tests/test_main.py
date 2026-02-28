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

# Now import the app
from app.main import app

# Restore threading
threading.Thread = _orig_thread

client = TestClient(app)


def test_root_serves_html():
  response = client.get("/")
  assert response.status_code == 200
  assert "Emergency Dispatcher Simulation" in response.text


def test_graph_and_state_endpoints():
  g = client.get("/graph")
  assert g.status_code == 200
  assert "nodes" in g.json() and "edges" in g.json()

  s = client.get("/state")
  assert s.status_code == 200
  assert "units" in s.json()

