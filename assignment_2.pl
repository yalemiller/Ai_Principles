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

% Helper predicate to get neighbors from the graph
to_edge((City, Neighbors), (City, Neighbor)) :- member((Neighbor, _), Neighbors).
edge(City1, City2) :-
    graph(Graph),
    member(Node, Graph),
    to_edge(Node, (City1, City2)).

% Helper predicate to get edge cost from the graph
edge_cost(City1, City2, Cost) :-
    graph(Graph),
    member((City1, Neighbors), Graph),
    member((City2, Cost), Neighbors).

% Calculate path cost
calculate_cost([], 0).
calculate_cost([_], 0).
calculate_cost([City1, City2 | Rest], TotalCost) :-
    edge_cost(City1, City2, Cost),
    calculate_cost([City2 | Rest], RestCost),
    TotalCost is Cost + RestCost.

% Count visited nodes
:- dynamic visited_counter/1.
init_counter :- retractall(visited_counter(_)), assertz(visited_counter(0)).
incr_counter :- retract(visited_counter(N)), N1 is N + 1, assertz(visited_counter(N1)).
get_counter(C) :- visited_counter(C).

% BFS implementation
bfs(Start, Goal, Path) :- 
    init_counter,
    bfs_queue([[Start]], Goal, Path).

% If the queue is empty, return an empty path
bfs_queue([], _, []).

% Process the queue
bfs_queue([[Goal | Rest] | _], Goal, Path) :-
    incr_counter,
    reverse([Goal | Rest], Path).

bfs_queue([[Current | Rest] | Queue], Goal, Path) :-
    incr_counter,
    findall([Next, Current | Rest], 
           (edge(Current, Next), \+ member(Next, [Current | Rest])), 
           Children),
    append(Queue, Children, NewQueue),
    bfs_queue(NewQueue, Goal, Path).




% DFS implementation
dfs(Start, Goal, Path) :- 
    init_counter,
    dfs_queue([[Start]], Goal, Path).

% If the queue is empty, return an empty path
dfs_queue([], _, []).

% Process the queue
dfs_queue([[Goal | Rest] | _], Goal, Path) :-
    incr_counter,
    reverse([Goal | Rest], Path).

dfs_queue([[Current | Rest] | Queue], Goal, Path) :-
    incr_counter,
    findall([Next, Current | Rest], 
           (edge(Current, Next), \+ member(Next, [Current | Rest])), 
           Children),
    append(Children, Queue, NewQueue), % Add to front for DFS
    dfs_queue(NewQueue, Goal, Path).




% Greedy search implementation
greedy(Start, Goal, Path) :-
    init_counter,
    heuristic(Start, H),
    greedy_queue([[[Start], H]], Goal, Path).

% If the queue is empty, return an empty path
greedy_queue([], _, []).

% Process the queue
greedy_queue([[[Goal | Rest], _] | _], Goal, Path) :-
    incr_counter,
    reverse([Goal | Rest], Path), 
    !.  % Ensure it stops here only when a valid path exists


greedy_queue([[[Current | Rest], _] | Queue], Goal, Path) :-
    incr_counter,
    findall([[Next, Current | Rest], H],
           (edge(Current, Next), 
            \+ member(Next, [Current | Rest]),
            heuristic(Next, H)),
           Children),
    insert_all(Children, Queue, NewQueue),
    greedy_queue(NewQueue, Goal, Path).

% Insert all children into the queue in sorted order
insert_all([], Queue, Queue).
insert_all([Child | Children], Queue, FinalQueue) :-
    insert_by_heuristic(Child, Queue, NewQueue),
    insert_all(Children, NewQueue, FinalQueue).

% Insert a child into the queue based on heuristic (sort in ascending order)
insert_by_heuristic([Path, H], [], [[Path, H]]) :- !.
insert_by_heuristic([Path, H], [[Path1, H1] | Rest], [[Path, H], [Path1, H1] | Rest]) :-
    H < H1, !.
insert_by_heuristic(Child, [First | Rest], [First | NewRest]) :-
    insert_by_heuristic(Child, Rest, NewRest).





% Run the algorithms and report results
run_test(Start, Goal) :-
    format('~nTesting paths from ~w to ~w:~n', [Start, Goal]),
    format('----------------------------------------~n'),
    
    % Breadth-first search
    init_counter,
    bfs(Start, Goal, BFSPath),
    get_counter(BFSVisited),
    calculate_cost(BFSPath, BFSCost),
    length(BFSPath, BFSLength),
    format('BFS Path: ~w~n', [BFSPath]),
    format('BFS Path Length: ~w~n', [BFSLength]),
    format('BFS Visited Nodes: ~w~n', [BFSVisited]),
    format('BFS Path Cost: ~w~n', [BFSCost]),
    format('----------------------------------------~n'),
    
    % Depth-first search
    init_counter,
    dfs(Start, Goal, DFSPath),
    get_counter(DFSVisited),
    calculate_cost(DFSPath, DFSCost),
    length(DFSPath, DFSLength),
    format('DFS Path: ~w~n', [DFSPath]),
    format('DFS Path Length: ~w~n', [DFSLength]),
    format('DFS Visited Nodes: ~w~n', [DFSVisited]),
    format('DFS Path Cost: ~w~n', [DFSCost]),
    format('----------------------------------------~n'),
    
    % Greedy search
    init_counter,
    greedy(Start, Goal, GreedyPath),
    get_counter(GreedyVisited),
    calculate_cost(GreedyPath, GreedyCost),
    length(GreedyPath, GreedyLength),
    format('Greedy Path: ~w~n', [GreedyPath]),
    format('Greedy Path Length: ~w~n', [GreedyLength]),
    format('Greedy Visited Nodes: ~w~n', [GreedyVisited]),
    format('Greedy Path Cost: ~w~n', [GreedyCost]),
    format('========================================~n').

% Run the assignment tests
run_assignment :-
    run_test("Oradea", "Bucharest"),
    run_test("Timisoara", "Bucharest"),
    run_test("Neamt", "Bucharest").
