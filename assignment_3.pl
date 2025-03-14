% permute_list/2: Produces all possible rearrangements of a list
% Useful for comparing states without considering order of elements
permute_list([], []).
permute_list(OrigList, [Selected|Rest]) :- 
    append(Front, [Selected|Back], OrigList),
    append(Front, Back, ReducedList),
    permute_list(ReducedList, Rest).

% exclude_element/3: Removes an item from a given list
exclude_element([], _, []).
exclude_element([Item|Remaining], Item, Remaining) :- !.
exclude_element([Head|Tail], Item, [Head|Modified]) :-  
    exclude_element(Tail, Item, Modified).

% goal_satisfied/1: Confirms if a given state matches the target configuration
goal_satisfied(CurrentState) :-
    target(_, ExpectedState),
    length(CurrentState, L),
    length(ExpectedState, L),
    permute_list(CurrentState, Reordered),
    Reordered = ExpectedState, !.


% defined_blocks/1: Lists valid block items
defined_blocks([a, b, c, d, e, f]).

% is_block/1: Validates if an item is a recognized block
is_block(Element):-
    defined_blocks(BlockSet),
    member(Element, BlockSet).

% distinct/2: Ensures two values are not identical
distinct(A, B) :- \+ A = B.

% substitute/4: Replaces an occurrence of one element with another in a list
substitute(Old, New, [Old|Rest], [New|Rest]) :- !.
substitute(Old, New, [Head|Tail1], [Head|Tail2]) :-
    substitute(Old, New, Tail1, Tail2).

% move_block/5: Defines permissible block relocations
move_block(Block, From, To, InitialState, UpdatedState):-  
    member([clear, Block], InitialState),
    member([on, Block, From], InitialState),
    is_block(From),
    member([clear, To], InitialState),
    distinct(Block, To),
    distinct(From, To),
    substitute([on, Block, From], [on, Block, To], InitialState, TempState),
    substitute([clear, To], [clear, From], TempState, UpdatedState).

move_block(Block, From, "table", InitialState, UpdatedState):-
    member([clear, Block], InitialState),
    member([on, Block, From], InitialState),
    is_block(From),
    substitute([on, Block, From], [clear, From], InitialState, TempState),
    append([[on, Block, "table"]], TempState, UpdatedState).

move_block(Block, "table", To, InitialState, UpdatedState):-
    member([clear, Block], InitialState),
    member([on, Block, "table"], InitialState),
    member([clear, To], InitialState),
    is_block(To),
    distinct(Block, To),
    substitute([on, Block, "table"], [on, Block, To], InitialState, TempState),
    exclude_element(TempState, [clear, To], UpdatedState).

% transition/2: Establishes legal transformations between states
transition(StartState, EndState):-
    move_block(_, _, _, StartState, EndState).

% avoid_repeats/2: Detects cycles in state traversal
avoid_repeats(State, VisitedStates) :-
    \+ (member(VisitedState, VisitedStates),
        length(State, Size),
        length(VisitedState, Size),
        permute_list(State, VisitedState)).

% traverse/3: Implements depth-first search with cycle prevention
traverse(Current, [Current], _) :- 
    goal_satisfied(Current), !.
traverse(Current, [Current|RemainingPath], Visited) :-
    transition(Current, NextState),
    avoid_repeats(NextState, Visited),
    traverse(NextState, RemainingPath, [NextState|Visited]).

% find_solution/2: Retrieves a valid path from initial state to goal
find_solution(StartingState, SolutionPath) :-
    traverse(StartingState, SolutionPath, [StartingState]).

% Display the steps to the solution
show_solution(SolutionPath) :-
    display_steps(SolutionPath, 0).


display_steps([], _).
display_steps([State|Remaining], StepNum) :-
    format('Step ~w:~n', [StepNum]),
    display_state(State),
    nl,
    NextStep is StepNum + 1,
    display_steps(Remaining, NextStep).

display_state([]).
display_state([Fact|Remaining]) :-
    format('  ~w~n', [Fact]),
    display_state(Remaining).

% Test Cases
:- discontiguous origin/2.
:- discontiguous target/2.


% Test 1: Simple stack reversal
origin(1, [[on, a, b], [on, b, "table"], [clear, a]]).
target(1, [[on, b, a], [on, a, "table"], [clear, b]]).

% Test 2: Interleaved stacks into a single stack
origin(2, [[on, a, b], [on, b, "table"], [on, c, d], [on, d, "table"], [clear, a], [clear, c]]).
target(2, [[on, a, b], [on, b, c], [on, c, d], [on, d, "table"], [clear, a]]).

% Test 3: Another initial and goal state example
origin(3, [[on, a, b], [on, b, "table"], [on, c, d], [clear, c], [clear, a], [on, d, "table"]]).
target(3, [[on, d, a], [on, a, c], [on, c, b], [on, b, "table"], [clear, d]]).

% Run all tests with a single command
run_tests :-
    forall(
        (between(1, 3, N), origin(N, O), target(N, T)), 
        (   format('Running Test ~w...~n', [N]),
            find_solution(O, P),
            show_solution(P)
        )
    ).
