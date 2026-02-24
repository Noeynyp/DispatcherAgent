from dataclasses import dataclass
from typing import List, Tuple, Optional
import math
import networkx as nx


@dataclass
class UnitState:
    id: str
    service: str
    path: List[str]
    edge_index: int = 0
    progress: float = 0.0
    speed: float = 20.0  # units per second

    def current_edge(self) -> Optional[Tuple[str, str]]:
        if self.edge_index + 1 < len(self.path):
            return (self.path[self.edge_index], self.path[self.edge_index + 1])
        return None


def euclid(a: Tuple[float, float], b: Tuple[float, float]) -> float:
    return math.hypot(b[0] - a[0], b[1] - a[1])


def compute_position(unit: UnitState, G: nx.Graph) -> Tuple[float, float]:
    edge = unit.current_edge()
    if edge is None:
        return G.nodes[unit.path[-1]]["pos"]

    a = G.nodes[edge[0]]["pos"]
    b = G.nodes[edge[1]]["pos"]

    return (
        a[0] + unit.progress * (b[0] - a[0]),
        a[1] + unit.progress * (b[1] - a[1]),
    )


def advance_unit(unit: UnitState, dt: float, G: nx.Graph):
    edge = unit.current_edge()
    if edge is None:
        return

    a = G.nodes[edge[0]]["pos"]
    b = G.nodes[edge[1]]["pos"]
    edge_len = euclid(a, b)

    if edge_len == 0:
        return

    distance = unit.speed * dt
    frac = distance / edge_len
    unit.progress += frac

    while unit.progress >= 1.0 and unit.edge_index + 1 < len(unit.path) - 1:
        unit.progress -= 1.0
        unit.edge_index += 1

    if unit.progress >= 1.0:
        unit.progress = 0.0
        unit.edge_index = len(unit.path) - 1


def remaining_time_to_node(unit: UnitState, target_node: str, G: nx.Graph) -> float:
    edge = unit.current_edge()
    remaining_distance = 0.0

    if edge is None:
        start_node = unit.path[-1]
    else:
        a = G.nodes[edge[0]]["pos"]
        b = G.nodes[edge[1]]["pos"]
        edge_len = euclid(a, b)
        remaining_distance += (1.0 - unit.progress) * edge_len
        start_node = edge[1]

    try:
        path = nx.shortest_path(G, start_node, target_node, weight="weight")
        for u, v in zip(path, path[1:]):
            remaining_distance += G.edges[u, v]["weight"]
    except nx.NetworkXNoPath:
        return float("inf")

    if unit.speed <= 0:
        return float("inf")

    return remaining_distance / unit.speed


def snap_to_nearest_node(pos: Tuple[float, float], G: nx.Graph) -> str:
    best_node = None
    best_dist = float("inf")

    for node, data in G.nodes(data=True):
        dist = euclid(pos, data["pos"])
        if dist < best_dist:
            best_dist = dist
            best_node = node

    return best_node


def create_sample_graph() -> nx.Graph:
    G = nx.Graph()

    G.add_node("n1", pos=(0, 0))
    G.add_node("n2", pos=(100, 0))
    G.add_node("n3", pos=(100, 100))
    G.add_node("n4", pos=(0, 100))

    for a, b in [("n1", "n2"), ("n2", "n3"), ("n3", "n4"), ("n4", "n1"), ("n1", "n3")]:
        pa = G.nodes[a]["pos"]
        pb = G.nodes[b]["pos"]
        G.add_edge(a, b, weight=euclid(pa, pb))

    return G