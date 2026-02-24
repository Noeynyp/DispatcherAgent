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



@app.get("/", response_class=HTMLResponse)
def read_root() -> str:
    """Serve the main HTML page."""
    index_path = STATIC_DIR / "index.html"
    return index_path.read_text(encoding="utf-8")


