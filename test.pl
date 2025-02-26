:- dynamic graph/1.

% Graph definition
graph([
    ("Arad", [("Zerind", 75), ("Timisoara", 118), ("Sibiu", 140)]),
    ("Bucharest", [("Fagaras", 211), ("Pitesti", 101), ("Giurgiu", 90), ("Urziceni", 85)]),
    ("Craiova", [("Drobeta", 120), ("Rimnicu Vilcea", 146), ("Pitesti", 138)]),
    ("Drobeta", [("Mehadia", 75), ("Craiova", 120)]),
    ("Eforie", [("Hirsova", 86)]),
    ("Fagaras", [("Sibiu", 99), ("Bucharest", 211)]),
    ("Giurgiu", [("Bucharest", 90)]),
    ("Hirsova", [("Urziceni", 98)]),
    ("Iasi", [("Vaslui", 92), ("Neamt", 87)]),
    ("Lugoj", [("Timisoara", 111), ("Mehadia", 70)]),
    ("Mehadia", [("Lugoj", 70), ("Drobeta", 75)]),
    ("Neamt", [("Iasi", 87)]),
    ("Oradea", [("Zerind", 71), ("Sibiu", 151)]),
    ("Pitesti", [("Rimnicu Vilcea", 97), ("Craiova", 138), ("Bucharest", 101)]),
    ("Rimnicu Vilcea", [("Sibiu", 80), ("Craiova", 146), ("Pitesti", 97)]),
    ("Sibiu", [("Arad", 140), ("Oradea", 151), ("Fagaras", 99), ("Rimnicu Vilcea", 80)]),
    ("Timisoara", [("Arad", 118), ("Lugoj", 111)]),
    ("Urziceni", [("Bucharest", 85), ("Vaslui", 142), ("Hirsova", 98)]),
    ("Vaslui", [("Iasi", 92), ("Urziceni", 142)]),
    ("Zerind", [("Arad", 75), ("Oradea", 71)])
]).

% Heuristic function
heuristic("Arad", 366).
heuristic("Bucharest", 0).
heuristic("Craiova", 160).
heuristic("Drobeta", 242).
heuristic("Eforie", 161).
heuristic("Fagaras", 176).
heuristic("Giurgiu", 77).
heuristic("Hirsova", 151).
heuristic("Iasi", 226).
heuristic("Lugoj", 244).
heuristic("Mehadia", 241).
heuristic("Neamt", 234).
heuristic("Oradea", 380).
heuristic("Pitesti", 100).
heuristic("Rimnicu Vilcea", 193).
heuristic("Sibiu", 253).
heuristic("Timisoara", 329).
heuristic("Urziceni", 80).
heuristic("Vaslui", 199).
heuristic("Zerind", 374).



% BFS implementation
bfs(Start, Goal, Path) :- bfs_queue([[Start]], Goal, Path).

% If the queue is empty, return an empty path
bfs_queue([], _, []).

% Process the queue
bfs_queue([[Goal | Rest] | _], Goal, Path) :-
    reverse([Goal | Rest], Path).

bfs_queue([[Current | Rest] | Queue], Goal, Path) :-
    findall([Next, Current | Rest], (edge(Current, Next), \+ member(Next, [Current | Rest])), Children),
    append(Queue, Children, NewQueue),
    bfs_queue(NewQueue, Goal, Path).

% Helper predicate to get neighbors from the graph
to_edge((City, Neighbors), (City, Neighbor)) :- member((Neighbor, _), Neighbors).
edge(City1, City2) :-
    graph(Graph),
    member(Node, Graph),
    to_edge(Node, (City1, City2)).