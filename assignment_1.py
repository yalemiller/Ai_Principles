#pip install networkx matplotlib
import heapq
import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
import timeit

bfs_CitiesVisited = 0
dfs_CitiesVisited = 0
best_CitiesVisited = 0
a_CitiesVisited = 0


def reset_globals():
    global bfs_CitiesVisited
    global dfs_CitiesVisited
    global best_CitiesVisited
    global a_CitiesVisited
    bfs_CitiesVisited = 0
    dfs_CitiesVisited = 0
    best_CitiesVisited = 0
    a_CitiesVisited = 0
    return None


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


def best_first_search(graph, start, goal, book_heuristic):
    global best_CitiesVisited
    pq = [(book_heuristic[start], start, [start])]
    visited = set()

    while pq:
        best_CitiesVisited += 1
        _, node, path = heapq.heappop(pq)
        if node == goal:
            return path
        if node not in visited:
            visited.add(node)
            for neighbor in graph[node]:
                heapq.heappush(
                    pq, (book_heuristic[neighbor], neighbor, path + [neighbor])
                )
    return None


def a_star_search(graph, start, goal, book_heuristic):
    global a_CitiesVisited
    pq = [(0 + book_heuristic[start], 0, start, [start])]
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
                        new_cost + book_heuristic[neighbor],
                        new_cost,
                        neighbor,
                        path + [neighbor],
                    ),
                )
    return None


def run_all_algoritims(start, goal, heuristic):
    global bfs_CitiesVisited
    global dfs_CitiesVisited
    global best_CitiesVisited
    global a_CitiesVisited
    bfs(graph, start, goal)
    dfs(graph, start, goal)
    best_first_search(graph, start, goal, heuristic)
    a_star_search(graph, start, goal, heuristic)
    return None


graph = {
    "Arad": {"Zerind": 75, "Timisoara": 118, "Sibiu": 140},
    "Bucharest": {"Fagaras": 211, "Pitesti": 101, "Giurgiu": 90, "Urziceni": 85},
    "Craiova": {"Drobeta": 120, "Rimnicu Vilcea": 146, "Pitesti": 138},
    "Drobeta": {"Mehadia": 75, "Craiova": 120},
    "Eforie": {"Hirsova": 86},
    "Fagaras": {"Sibiu": 99, "Bucharest": 211},
    "Giurgiu": {"Bucharest": 90},
    "Hirsova": {"Urziceni": 98},
    "Iasi": {"Vaslui": 92, "Neamt": 87},
    "Lugoj": {"Timisoara": 111, "Mehadia": 70},
    "Mehadia": {"Lugoj": 70, "Drobeta": 75},
    "Neamt": {"Iasi": 87},
    "Oradea": {"Zerind": 71, "Sibiu": 151},
    "Pitesti": {"Rimnicu Vilcea": 97, "Craiova": 138, "Bucharest": 101},
    "Rimnicu Vilcea": {"Sibiu": 80, "Craiova": 146, "Pitesti": 97},
    "Sibiu": {"Arad": 140, "Oradea": 151, "Fagaras": 99, "Rimnicu Vilcea": 80},
    "Timisoara": {"Arad": 118, "Lugoj": 111},
    "Urziceni": {"Bucharest": 85, "Vaslui": 142, "Hirsova": 98},
    "Vaslui": {"Iasi": 92, "Urziceni": 142},
    "Zerind": {"Arad": 75, "Oradea": 71},
}


book_heuristic = {
    "Arad": 366,
    "Bucharest": 0,
    "Craiova": 160,
    "Drobeta": 242,
    "Eforie": 161,
    "Fagaras": 176,
    "Giurgiu": 77,
    "Hirsova": 151,
    "Iasi": 226,
    "Lugoj": 244,
    "Mehadia": 241,
    "Neamt": 234,
    "Oradea": 380,
    "Pitesti": 100,
    "Rimnicu Vilcea": 193,
    "Sibiu": 253,
    "Timisoara": 329,
    "Urziceni": 80,
    "Vaslui": 199,
    "Zerind": 374,
}

method_1_heuristic = {
    "Arad": 466,
    "Bucharest": 100,
    "Craiova": 260,
    "Drobeta": 342,
    "Eforie": 261,
    "Fagaras": 276,
    "Giurgiu": 177,
    "Hirsova": 251,
    "Iasi": 326,
    "Lugoj": 344,
    "Mehadia": 341,
    "Neamt": 334,
    "Oradea": 480,
    "Pitesti": 0,
    "Rimnicu Vilcea": 293,
    "Sibiu": 353,
    "Timisoara": 429,
    "Urziceni": 180,
    "Vaslui": 299,
    "Zerind": 474,
}

method_2_heuristic = {
    "Arad": 239,
    "Bucharest": 211,
    "Craiova": 325,
    "Drobeta": 445,
    "Eforie": 480,
    "Fagaras": 0,
    "Giurgiu": 301,
    "Hirsova": 394,
    "Iasi": 530,
    "Lugoj": 468,
    "Mehadia": 520,
    "Neamt": 617,
    "Oradea": 250,
    "Pitesti": 276,
    "Rimnicu Vilcea": 179,
    "Sibiu": 99,
    "Timisoara": 357,
    "Urziceni": 296,
    "Vaslui": 438,
    "Zerind": 314,
}


# --- Create Plot ---
plt.figure(figsize=(18, 10))

# --- First Graph (Cities Visited, Arad to Bucharest, Book Heuristic) ---
start, goal = "Arad", "Bucharest"
run_all_algoritims(start, goal, book_heuristic)

labels_cities = ["BFS", "DFS", "Best", "A*"]
values_cities = [
    bfs_CitiesVisited,
    dfs_CitiesVisited,
    best_CitiesVisited,
    a_CitiesVisited,
]

plt.subplot(2, 3, 1)  # Row 1, Column 1
plt.bar(labels_cities, values_cities)
plt.title(f"Cities Visited, {start} to {goal} (Book)")
plt.xlabel("Algorithm")
plt.ylabel("Cities Visited")
plt.gca().yaxis.set_major_locator(MaxNLocator(integer=True))

# --- Second Graph (Cities Visited, Timisoara to Pitesti, Method 1) ---
reset_globals()
start, goal = "Timisoara", "Pitesti"
run_all_algoritims(start, goal, method_1_heuristic)

values_cities = [
    bfs_CitiesVisited,
    dfs_CitiesVisited,
    best_CitiesVisited,
    a_CitiesVisited,
]

plt.subplot(2, 3, 2)
plt.bar(labels_cities, values_cities)
plt.title(f"Cities Visited, {start} to {goal} (Method 1)")
plt.xlabel("Algorithm")
plt.ylabel("Cities Visited")
plt.gca().yaxis.set_major_locator(MaxNLocator(integer=True))

# --- Third Graph (Cities Visited, Drobeta to Fagaras, Method 2) ---
reset_globals()
start, goal = "Drobeta", "Fagaras"
run_all_algoritims(start, goal, method_2_heuristic)

values_cities = [
    bfs_CitiesVisited,
    dfs_CitiesVisited,
    best_CitiesVisited,
    a_CitiesVisited,
]

plt.subplot(2, 3, 3)
plt.bar(labels_cities, values_cities)
plt.title(f"Cities Visited, {start} to {goal} (Method 2)")
plt.xlabel("Algorithm")
plt.ylabel("Cities Visited")
plt.gca().yaxis.set_major_locator(MaxNLocator(integer=True))

# --- Fourth Graph (Time Taken, Arad to Bucharest, Book Heuristic) ---
start, goal = "Arad", "Bucharest"
heuristic = book_heuristic

time_bfs = timeit.timeit(
    stmt="bfs(graph, start, goal)", globals=globals(), number=10000
)
time_dfs = timeit.timeit(
    stmt="dfs(graph, start, goal)", globals=globals(), number=10000
)
time_best = timeit.timeit(
    stmt="best_first_search(graph, start, goal, heuristic)",
    globals=globals(),
    number=10000,
)
time_A = timeit.timeit(
    stmt="a_star_search(graph, start, goal, heuristic)", globals=globals(), number=10000
)

labels_time = ["BFS", "DFS", "Best", "A*"]
values_time = [time_bfs, time_dfs, time_best, time_A]

plt.subplot(2, 3, 4)  # Row 2, Column 1
plt.bar(labels_time, values_time)
plt.title(f"Time Taken, {start} to {goal} (Book)")
plt.xlabel("Algorithm")
plt.ylabel("Time")

# --- Fifth Graph (Time Taken, Timisoara to Pitesti, Method 1) ---
start, goal = "Timisoara", "Pitesti"
heuristic = method_1_heuristic

time_bfs = timeit.timeit(
    stmt="bfs(graph, start, goal)", globals=globals(), number=10000
)
time_dfs = timeit.timeit(
    stmt="dfs(graph, start, goal)", globals=globals(), number=10000
)
time_best = timeit.timeit(
    stmt="best_first_search(graph, start, goal, heuristic)",
    globals=globals(),
    number=10000,
)
time_A = timeit.timeit(
    stmt="a_star_search(graph, start, goal, heuristic)", globals=globals(), number=10000
)

values_time = [time_bfs, time_dfs, time_best, time_A]

plt.subplot(2, 3, 5)
plt.bar(labels_time, values_time)
plt.title(f"Time Taken, {start} to {goal} (Method 1)")
plt.xlabel("Algorithm")
plt.ylabel("Time")

# --- Sixth Graph (Time Taken, Drobeta to Fagaras, Method 2) ---
start, goal = "Drobeta", "Fagaras"
heuristic = method_2_heuristic

time_bfs = timeit.timeit(
    stmt="bfs(graph, start, goal)", globals=globals(), number=10000
)
time_dfs = timeit.timeit(
    stmt="dfs(graph, start, goal)", globals=globals(), number=10000
)
time_best = timeit.timeit(
    stmt="best_first_search(graph, start, goal, heuristic)",
    globals=globals(),
    number=10000,
)
time_A = timeit.timeit(
    stmt="a_star_search(graph, start, goal, heuristic)", globals=globals(), number=10000
)

values_time = [time_bfs, time_dfs, time_best, time_A]

plt.subplot(2, 3, 6)  # Row 2, Column 3
plt.bar(labels_time, values_time)
plt.title(f"Time Taken, {start} to {goal} (Method 2)")
plt.xlabel("Algorithm")
plt.ylabel("Time")

plt.tight_layout()
plt.show()
