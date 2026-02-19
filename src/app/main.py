from pathlib import Path
from typing import List

from fastapi import FastAPI
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel


BASE_DIR = Path(__file__).resolve().parent
STATIC_DIR = BASE_DIR / "static"

app = FastAPI(title="Dispatcher System")

app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")


class IncidentCreate(BaseModel):
    caller_name: str
    location: str
    description: str


class Incident(IncidentCreate):
    id: int
    status: str


_INCIDENTS: List[Incident] = []
_NEXT_ID: int = 1


@app.get("/", response_class=HTMLResponse)
def read_root() -> str:
    """Serve the main HTML page."""
    index_path = STATIC_DIR / "index.html"
    return index_path.read_text(encoding="utf-8")


@app.get("/api/health")
def health_check() -> dict:
    """Simple health endpoint to test the API."""
    return {"status": "ok"}


@app.post("/api/incidents", response_model=Incident, status_code=201)
def create_incident(payload: IncidentCreate) -> Incident:
    """Create a new incident case."""
    global _NEXT_ID

    incident = Incident(
        id=_NEXT_ID,
        caller_name=payload.caller_name,
        location=payload.location,
        description=payload.description,
        status="new",
    )
    _NEXT_ID += 1
    _INCIDENTS.append(incident)
    return incident


@app.get("/api/incidents", response_model=List[Incident])
def list_incidents() -> List[Incident]:
    """Return all incident cases."""
    return _INCIDENTS


