import sys
import types

# Insert a simple fake pyswip module so the app can import without real SWI-Prolog
_pyswip = types.ModuleType("pyswip")
class FakeProlog:
    def __init__(self, *a, **k):
        pass
    def consult(self, *a, **k):
        return None
    def query(self, *a, **k):
        return []
_pyswip.Prolog = FakeProlog
import builtins
import sys
sys.modules['pyswip'] = _pyswip

# Run uvicorn programmatically to ensure the fake module is present before import
import uvicorn
from pathlib import Path
import importlib

# Ensure project root is on sys.path so `src` imports work
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

port = 8000
if len(sys.argv) > 1:
    try:
        port = int(sys.argv[1])
    except Exception:
        pass

# Import the app module directly so we can patch any missing globals before running
app_mod = importlib.import_module('src.app.main')
if not hasattr(app_mod, 'selected'):
    app_mod.selected = []

uvicorn.run(app_mod.app, host='127.0.0.1', port=port)
