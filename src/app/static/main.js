async function checkHealth() {
  const statusElement = document.getElementById("status");
  const dot = document.getElementById("health-dot");

  try {
    const response = await fetch("/api/health");
    if (!response.ok) {
      statusElement.textContent = "API error";
      dot.classList.remove("ok");
      dot.classList.add("bad");
      return;
    }
    const data = await response.json();
    statusElement.textContent = data.status === "ok" ? "Connected" : `API: ${data.status}`;
    dot.classList.remove("bad");
    dot.classList.add("ok");
  } catch (e) {
    statusElement.textContent = "Offline";
    dot.classList.remove("ok");
    dot.classList.add("bad");
  }
}

function statusBadge(status) {
  const span = document.createElement("span");
  span.className = `badge ${status}`;
  const dot = document.createElement("span");
  dot.className = "tiny-dot";
  const text = document.createElement("span");
  text.textContent = status.toUpperCase();
  span.appendChild(dot);
  span.appendChild(text);
  return span;
}

async function fetchIncidents() {
  const tbody = document.getElementById("incident-tbody");
  const empty = document.getElementById("queue-empty");
  const count = document.getElementById("queue-count");

  tbody.innerHTML = "";

  const response = await fetch("/api/incidents");
  if (!response.ok) {
    empty.hidden = false;
    empty.textContent = "Unable to fetch incidents.";
    count.textContent = "";
    return;
  }

  const incidents = await response.json();
  if (!incidents.length) {
    empty.hidden = false;
    empty.textContent = "No incidents logged yet.";
    count.textContent = "Queue: 0";
    return;
  }

  empty.hidden = true;
  count.textContent = `Queue: ${incidents.length}`;

  for (const incident of incidents) {
    const tr = document.createElement("tr");

    const idTd = document.createElement("td");
    idTd.className = "mono";
    idTd.textContent = `#${incident.id}`;

    const statusTd = document.createElement("td");
    statusTd.appendChild(statusBadge(incident.status || "new"));

    const callerTd = document.createElement("td");
    callerTd.textContent = incident.caller_name;

    const locationTd = document.createElement("td");
    locationTd.textContent = incident.location;

    const descTd = document.createElement("td");
    descTd.textContent = incident.description;

    tr.appendChild(idTd);
    tr.appendChild(statusTd);
    tr.appendChild(callerTd);
    tr.appendChild(locationTd);
    tr.appendChild(descTd);
    tbody.appendChild(tr);
  }
}

function setupIncidentForm() {
  const form = document.getElementById("incident-form");
  const message = document.getElementById("incident-message");
  const submit = document.getElementById("submit");

  form.addEventListener("submit", async (event) => {
    event.preventDefault();
    message.textContent = "";
    message.classList.remove("ok", "bad");

    const payload = {
      caller_name: form.caller_name.value,
      location: form.location.value,
      description: form.description.value,
    };

    submit.disabled = true;
    submit.textContent = "Creating…";

    const response = await fetch("/api/incidents", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(payload),
    });

    if (!response.ok) {
      message.textContent = "Failed to create incident.";
      message.classList.add("bad");
      submit.disabled = false;
      submit.textContent = "Create incident";
      return;
    }

    const created = await response.json();
    message.textContent = `Incident #${created.id} created and added to queue.`;
    message.classList.add("ok");
    form.reset();
    submit.disabled = false;
    submit.textContent = "Create incident";
    await fetchIncidents();
  });
}

function setupRefresh() {
  const refresh = document.getElementById("refresh");
  refresh.addEventListener("click", async () => {
    refresh.disabled = true;
    refresh.textContent = "Refreshing…";
    await fetchIncidents();
    refresh.disabled = false;
    refresh.textContent = "Refresh";
  });
}

checkHealth();
setupIncidentForm();
fetchIncidents();
setupRefresh();

// Light polling to keep queue current in multi-operator setups
setInterval(fetchIncidents, 8000);

