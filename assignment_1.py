import heapq
import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

__filename__ = "assignment_1.py"

bfs_CitiesVisited = 0
dfs_CitiesVisited = 0
greedy_CitiesVisited = 0
a_CitiesVisited = 0


def bfs(graph, start, goal):
    global bfs_CitiesVisited
    queue = [(start, [start])]
    visited = set()

    while queue:
        bfs_CitiesVisited += 1
        node, path = queue.pop(0)
        if node == goal:
            return path
        if node not in visited:
            visited.add(node)
            queue.extend((neighbor, path + [neighbor]) for neighbor in graph[node])
    return None


def dfs(graph, start, goal, path=None, visited=None):
    global dfs_CitiesVisited
    dfs_CitiesVisited += 1
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
    global greedy_CitiesVisited
    pq = [(heuristic[start], start, [start])]
    visited = set()

    while pq:
        greedy_CitiesVisited += 1
        _, node, path = heapq.heappop(pq)
        if node == goal:
            return path
        if node not in visited:
            visited.add(node)
            for neighbor in graph[node]:
                heapq.heappush(pq, (heuristic[neighbor], neighbor, path + [neighbor]))
    return None


def a_star_search(graph, start, goal, heuristic):
    global a_CitiesVisited
    pq = [(0 + heuristic[start], 0, start, [start])]
    visited = set()

    while pq:
        a_CitiesVisited += 1
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

start, goal = "Arad", "Bucharest"
print("BFS Path:", bfs(graph, start, goal))
print("DFS Path:", dfs(graph, start, goal))
print("Best-First Search Path:", best_first_search(graph, start, goal, heuristic))
print("A* Path:", a_star_search(graph, start, goal, heuristic))

labels = ["Breadth First", "Depth First", "Greedy", "A*"]
values = [bfs_CitiesVisited, dfs_CitiesVisited, greedy_CitiesVisited, a_CitiesVisited]

# Create the bar chart
plt.bar(labels, values)

# Add title and labels
plt.title("Number of Cities Visited")
plt.xlabel("Algorithm")
plt.ylabel("Cities Visited")

# Set the y-axis to display only whole numbers
plt.gca().yaxis.set_major_locator(MaxNLocator(integer=True))

# Show the chart
plt.show()
