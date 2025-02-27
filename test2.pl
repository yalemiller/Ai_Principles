:- discontiguous extend/2.

% Define the connections between cities with distances
road(arad, zerind, 75).
road(arad, sibiu, 140).
road(arad, timisoara, 118).
road(zerind, oradea, 71).
road(oradea, sibiu, 151).
road(timisoara, lugoj, 111).
road(lugoj, mehadia, 70).
road(mehadia, drobeta, 75).
road(drobeta, craiova, 120).
road(craiova, rimnicu_vilcea, 146).
road(craiova, pitesti, 138).
road(rimnicu_vilcea, sibiu, 80).
road(rimnicu_vilcea, pitesti, 97).
road(sibiu, fagaras, 99).
road(fagaras, bucharest, 211).
road(pitesti, bucharest, 101).
road(bucharest, giurgiu, 90).
road(bucharest, urziceni, 85).
road(urziceni, hirsova, 98).
road(hirsova, eforie, 86).
road(urziceni, vaslui, 142).
road(vaslui, iasi, 92).
road(iasi, neamt, 87).

% Heuristic values (straight-line distance to Bucharest)
h(arad, 366).
h(zerind, 374).
h(oradea, 380).
h(sibiu, 253).
h(timisoara, 329).
h(lugoj, 244).
h(mehadia, 241).
h(drobeta, 242).
h(craiova, 160).
h(rimnicu_vilcea, 193).
h(pitesti, 100).
h(fagaras, 176).
h(bucharest, 0).
h(giurgiu, 77).
h(urziceni, 80).
h(hirsova, 151).
h(eforie, 161).
h(vaslui, 199).
h(iasi, 226).
h(neamt, 234).

% Generic path rule that allows bidirectional traversal
path(X, Y, D) :- road(X, Y, D).
path(X, Y, D) :- road(Y, X, D).

% Breadth-First Search (BFS)
bfs(Start, Goal, Path, Cost) :- bfs_helper([[Start]], Goal, Path), path_length(Path, Cost).

bfs_helper([[Goal|Rest]|_], Goal, Path) :- reverse([Goal|Rest], Path).
bfs_helper([Path|Paths], Goal, Solution) :-
    extend(Path, NewPaths),
    append(Paths, NewPaths, UpdatedPaths),
    bfs_helper(UpdatedPaths, Goal, Solution).

extend([Current|Rest], NewPaths) :-
    findall([Next, Current|Rest], (path(Current, Next, _), \+ member(Next, [Current|Rest])), NewPaths).

% Depth-First Search (DFS)
dfs(Start, Goal, Path, Cost) :- dfs_helper(Start, Goal, [Start], RevPath), reverse(RevPath, Path), path_length(Path, Cost).

dfs_helper(Goal, Goal, Path, Path).  % If we reach the goal, return path
dfs_helper(Current, Goal, Visited, Path) :-
    findall(Next, (path(Current, Next, _), \+ member(Next, Visited)), NextNodes),
    sort(NextNodes, OrderedNodes),  % Ensure Pitesti is prioritized before Fagaras
    member(Next, OrderedNodes),  % Pick next node in order
    dfs_helper(Next, Goal, [Next|Visited], Path).

% Greedy Best-First Search (Uses Heuristic h(n))
greedy(Start, Goal, Path, Cost) :- greedy_helper([[Start]], Goal, Path), path_length(Path, Cost).

greedy_helper([[Goal|Rest]|_], Goal, Path) :- reverse([Goal|Rest], Path).
greedy_helper(Paths, Goal, Solution) :-
    best_first(Paths, BestPath, OtherPaths),
    extend(BestPath, NewPaths),
    append(NewPaths, OtherPaths, UpdatedPaths),
    greedy_helper(UpdatedPaths, Goal, Solution).

best_first(Paths, BestPath, OtherPaths) :-
    findall((H, P), (member(P, Paths), last(P, City), h(City, H)), HeuristicPaths),
    sort(HeuristicPaths, Sorted),
    Sorted = [( _, BestPath)|OtherPairs],
    findall(P, member((_, P), OtherPairs), OtherPaths).

% Path Length Calculation (Total Cost of Path)
path_length([_], 0).
path_length([A, B | Rest], TotalCost) :-
    path(A, B, Cost),
    path_length([B | Rest], RemainingCost),
    TotalCost is Cost + RemainingCost.

:- dynamic bfs/4, dfs/4, greedy/4.

% Commands to run: 
% cd('Desktop/folder').
% consult('Lab2.pl').
% bfs(bucharest, sibiu, Path, Cost).