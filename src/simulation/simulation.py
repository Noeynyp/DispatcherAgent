from dataclasses import dataclass, field
from typing import List, Tuple, Optional
import math
import networkx as nx


UNIT_SPEED = 20.0

@dataclass
class Building:
    id: str
    edge: Tuple[str, str]
    offset: float   # 0.0–1.0 along the road edge
    side: int       # +1 left of road direction, -1 right

@dataclass
class UnitState:
    id: str
    service: str
    current_node: str
    path: List[str] = field(default_factory=list)
    edge_index: int = 0
    progress: float = 0.0
    status: str = "idle"
    handling_remaining: float = 0.0

def euclid(a: Tuple[float, float], b: Tuple[float, float]) -> float:
    return math.hypot(b[0] - a[0], b[1] - a[1])


def compute_position(unit, G):
    if not unit.path or len(unit.path) < 2:
        return G.nodes[unit.current_node]["pos"]

    edge = (unit.path[unit.edge_index], unit.path[unit.edge_index + 1])
    a = G.nodes[edge[0]]["pos"]
    b = G.nodes[edge[1]]["pos"]

    x = a[0] + unit.progress * (b[0] - a[0])
    y = a[1] + unit.progress * (b[1] - a[1])
    return (x, y)


def advance_unit(unit, dt, G):

    # -------------------
    # HANDLING PHASE
    # -------------------
    if unit.status == "handling":
        unit.handling_remaining -= dt

        if unit.handling_remaining <= 0:
            unit.status = "idle"
            return ("finished_handling", unit.id)

        return None

    # -------------------
    # MOVEMENT PHASE
    # -------------------
    if not unit.path or len(unit.path) < 2:
        return None

    edge = (unit.path[unit.edge_index], unit.path[unit.edge_index + 1])
    a = G.nodes[edge[0]]["pos"]
    b = G.nodes[edge[1]]["pos"]

    edge_length = ((b[0] - a[0])**2 + (b[1] - a[1])**2) ** 0.5

    if edge_length == 0:
        return None

    unit.progress += (UNIT_SPEED * dt) / edge_length

    if unit.progress >= 1.0:
        unit.progress = 0.0
        unit.edge_index += 1

        if unit.edge_index >= len(unit.path) - 1:
            unit.current_node = unit.path[-1]
            unit.path = []
            return ("arrived", unit.id)

    return None

def remaining_time_to_node(unit, target_node, G):

    # If unit is idle → start from its current node
    if len(unit.path) < 2:
        start_node = unit.current_node
        try:
            path = nx.shortest_path(G, start_node, target_node, weight="weight")
            distance = 0.0
            for a, b in zip(path, path[1:]):
                distance += G.edges[a, b]["weight"]
            return distance / UNIT_SPEED
        except nx.NetworkXNoPath:
            return float("inf")

    # If unit is moving
    a = unit.path[unit.edge_index]
    b = unit.path[unit.edge_index + 1]

    pos_a = G.nodes[a]["pos"]
    pos_b = G.nodes[b]["pos"]

    edge_length = math.dist(pos_a, pos_b)
    remaining_edge_distance = (1 - unit.progress) * edge_length

    remaining_path_distance = 0.0

    if unit.edge_index + 1 < len(unit.path) - 1:
        next_node = unit.path[unit.edge_index + 1]
        remaining_nodes = unit.path[unit.edge_index + 1:]

        for x, y in zip(remaining_nodes, remaining_nodes[1:]):
            remaining_path_distance += G.edges[x, y]["weight"]
    else:
        next_node = unit.path[-1]

    try:
        path = nx.shortest_path(G, next_node, target_node, weight="weight")
        extra_distance = 0.0
        for a, b in zip(path, path[1:]):
            extra_distance += G.edges[a, b]["weight"]
    except nx.NetworkXNoPath:
        return float("inf")

    total_distance = remaining_edge_distance + remaining_path_distance + extra_distance

    return total_distance / UNIT_SPEED


def snap_to_nearest_node(pos: Tuple[float, float], G: nx.Graph) -> str:
    """Snap to nearest intersection node only — building nodes are excluded."""
    best_node = None
    best_dist = float("inf")

    for node, data in G.nodes(data=True):
        if data.get("type") == "building":
            continue
        dist = euclid(pos, data["pos"])
        if dist < best_dist:
            best_dist = dist
            best_node = node

    return best_node


def create_sample_graph() -> nx.Graph:
    G = nx.Graph()

    size = 5
    spacing = 100

    for i in range(size):
        for j in range(size):
            node_id = f"n{i}_{j}"
            G.add_node(node_id, pos=(i * spacing, j * spacing), type="intersection")

    for i in range(size):
        for j in range(size):
            node = f"n{i}_{j}"

            if i < size - 1:
                right = f"n{i+1}_{j}"
                pa = G.nodes[node]["pos"]
                pb = G.nodes[right]["pos"]
                G.add_edge(node, right, weight=euclid(pa, pb))

            if j < size - 1:
                up = f"n{i}_{j+1}"
                pa = G.nodes[node]["pos"]
                pb = G.nodes[up]["pos"]
                G.add_edge(node, up, weight=euclid(pa, pb))

    return G


def add_buildings(G: nx.Graph, buildings_per_side: int = 1) -> List[Building]:
    """
    Add building nodes to the graph along each road edge.
    Buildings appear on both sides of the road. Each building is a real graph node
    connected by a short edge to its nearest intersection, so units can navigate to them.
    """
    buildings = []
    bid = 0
    road_offset = 14        # perpendicular distance from road center (SVG units)
    offsets = [0.5] if buildings_per_side == 1 else [0.3, 0.7]

    for u, v in list(G.edges()):
        # Only place buildings on road edges between intersections
        if G.nodes[u].get("type") != "intersection" or G.nodes[v].get("type") != "intersection":
            continue

        ux, uy = G.nodes[u]["pos"]
        vx, vy = G.nodes[v]["pos"]
        dx, dy = vx - ux, vy - uy
        length = math.hypot(dx, dy)
        perp_x = -dy / length
        perp_y = dx / length

        for offset in offsets:
            for side in [+1, -1]:
                b_id = f"b{bid}"

                # Position along the road at this offset
                px = ux + offset * dx
                py = uy + offset * dy

                # Shift perpendicular to the road for visual placement
                bx = px + side * road_offset * perp_x
                by = py + side * road_offset * perp_y

                # Connect to BOTH intersection endpoints so units approaching
                # from either direction don't have to overshoot and double back.
                dist_to_u = math.hypot(bx - ux, by - uy)
                dist_to_v = math.hypot(bx - vx, by - vy)

                G.add_node(b_id, pos=(bx, by), type="building")
                G.add_edge(b_id, u, weight=dist_to_u)
                G.add_edge(b_id, v, weight=dist_to_v)

                buildings.append(Building(id=b_id, edge=(u, v), offset=offset, side=side))
                bid += 1

    return buildings