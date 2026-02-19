from fastapi.testclient import TestClient

from app.main import app, _INCIDENTS  # type: ignore


client = TestClient(app)


def setup_function() -> None:
    # Reset in-memory store between tests
    _INCIDENTS.clear()


def test_create_incident():
    payload = {
        "caller_name": "Alice",
        "location": "Main St & 3rd",
        "description": "Car accident, two vehicles involved",
    }

    response = client.post("/api/incidents", json=payload)
    assert response.status_code == 201

    data = response.json()
    assert data["id"] == 1
    assert data["caller_name"] == payload["caller_name"]
    assert data["location"] == payload["location"]
    assert data["description"] == payload["description"]
    assert data["status"] == "new"


def test_list_incidents_after_creation():
    # Arrange: create one incident
    create_payload = {
        "caller_name": "Bob",
        "location": "5th Ave",
        "description": "Medical emergency",
    }
    create_response = client.post("/api/incidents", json=create_payload)
    assert create_response.status_code == 201

    # Act: list incidents
    list_response = client.get("/api/incidents")
    assert list_response.status_code == 200

    incidents = list_response.json()
    assert len(incidents) == 1
    assert incidents[0]["caller_name"] == "Bob"
    assert incidents[0]["status"] == "new"

