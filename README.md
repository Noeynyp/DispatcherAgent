# Dispatcher System Starter

This is a minimal starter project for a dispatcher system web app.

### Tech stack
- **Backend**: `FastAPI`
- **Frontend**: Simple `HTML` + `JavaScript`
- **Tests**: `pytest`

### Project structure
- `src/` – application source code
  - `app/` – FastAPI backend and static frontend assets
- `tests/` – automated tests

### Setup
```bash
python -m venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate
pip install -r requirements.txt
```

### Run the server
```bash
uvicorn app.main:app --reload --app-dir src
```

Server will be available at `http://127.0.0.1:8000`.

### Run tests
```bash
pytest
```

### Run tests with coverage
```bash
pytest --cov=src --cov-report=term-missing
```

