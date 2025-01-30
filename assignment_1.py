import heapq
import networkx as nx
import matplotlib.pyplot as plt


def bfs(graph, start, goal):
    queue = [(start, [start])]
    visited = set()

    while queue:
        node, path = queue.pop(0)
        if node == goal:
            return path
        if node not in visited:
            visited.add(node)
            queue.extend((neighbor, path + [neighbor]) for neighbor in graph[node])
    return None


def dfs(graph, start, goal, path=None, visited=None):
    if path is None:
        path = [start]
    if visited is None:
        visited = set()

    if start == goal:
        return path
    visited.add(start)

    for neighbor in graph[start]:
        if neighbor not in visited:
            new_path = dfs(graph, neighbor, goal, path + [neighbor], visited)
            if new_path:
                return new_path
    return None


def best_first_search(graph, start, goal, heuristic):
    pq = [(heuristic[start], start, [start])]
    visited = set()

    while pq:
        _, node, path = heapq.heappop(pq)
        if node == goal:
            return path
        if node not in visited:
            visited.add(node)
            for neighbor in graph[node]:
                heapq.heappush(pq, (heuristic[neighbor], neighbor, path + [neighbor]))
    return None


def a_star_search(graph, start, goal, heuristic):
    pq = [(0 + heuristic[start], 0, start, [start])]
    visited = set()

    while pq:
        _, cost, node, path = heapq.heappop(pq)
        if node == goal:
            return path
        if node not in visited:
            visited.add(node)
            for neighbor, weight in graph[node].items():
                new_cost = cost + weight
                heapq.heappush(
                    pq,
                    (
                        new_cost + heuristic[neighbor],
                        new_cost,
                        neighbor,
                        path + [neighbor],
                    ),
                )
    return None


# Example adjacency list for the Romanian road map
graph = {
    "Arad": {"Zerind": 75, "Timisoara": 118, "Sibiu": 140},
    "Zerind": {"Arad": 75, "Oradea": 71},
    "Oradea": {"Zerind": 71, "Sibiu": 151},
    "Sibiu": {"Arad": 140, "Oradea": 151, "Fagaras": 99, "Rimnicu Vilcea": 80},
    "Timisoara": {"Arad": 118, "Lugoj": 111},
    "Lugoj": {"Timisoara": 111, "Mehadia": 70},
    "Mehadia": {"Lugoj": 70, "Drobeta": 75},
    "Drobeta": {"Mehadia": 75, "Craiova": 120},
    "Craiova": {"Drobeta": 120, "Rimnicu Vilcea": 146, "Pitesti": 138},
    "Rimnicu Vilcea": {"Sibiu": 80, "Craiova": 146, "Pitesti": 97},
    "Fagaras": {"Sibiu": 99, "Bucharest": 211},
    "Pitesti": {"Rimnicu Vilcea": 97, "Craiova": 138, "Bucharest": 101},
    "Bucharest": {"Fagaras": 211, "Pitesti": 101, "Giurgiu": 90},
    "Giurgiu": {"Bucharest": 90},
}

# Example heuristic (Straight-Line Distance to Bucharest)
heuristic = {
    "Arad": 366,
    "Zerind": 374,
    "Oradea": 380,
    "Sibiu": 253,
    "Timisoara": 329,
    "Lugoj": 244,
    "Mehadia": 241,
    "Drobeta": 242,
    "Craiova": 160,
    "Rimnicu Vilcea": 193,
    "Fagaras": 178,
    "Pitesti": 98,
    "Bucharest": 0,
    "Giurgiu": 77,
}


# Function to visualize the graph
def draw_graph(graph):
    G = nx.Graph()
    for node, edges in graph.items():
        for neighbor, weight in edges.items():
            G.add_edge(node, neighbor, weight=weight)

    pos = nx.spring_layout(G)
    labels = nx.get_edge_attributes(G, "weight")
    nx.draw(
        G,
        pos,
        with_labels=True,
        node_color="lightblue",
        edge_color="gray",
        node_size=2000,
        font_size=10,
    )
    nx.draw_networkx_edge_labels(G, pos, edge_labels=labels)
    plt.show()


# Testing the functions
start, goal = "Arad", "Bucharest"
print("BFS Path:", bfs(graph, start, goal))
print("DFS Path:", dfs(graph, start, goal))
print("Best-First Search Path:", best_first_search(graph, start, goal, heuristic))
print("A* Path:", a_star_search(graph, start, goal, heuristic))

draw_graph(graph)
