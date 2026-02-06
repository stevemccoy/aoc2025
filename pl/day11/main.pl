/* -*- prolog-mode -*- */
% Advent of Code 2025 - Day 11: Reactor

% solve(Node, Solution)
%	Solution is an acyclic path (in reverse order) between Node and a goal

solve(Node, Goal, Solution) :-
	depthfirst([], Node, Goal, Solution).

% depthfirst(Path, Node, Goal, Solution)
%	extending the path [Node | Path] to a goal gives Solution

depthfirst(Path, Node, Node, [Node | Path]).
depthfirst(Path, Node, Goal, Sol) :-
	s(Node, Node1),
	not(member(Node1, Path)),	% Prevent cycles.
	depthfirst([Node | Path], Node1, Goal, Sol).


path(Node, Node, [Node]).
path(FirstNode, LastNode, [LastNode | Path]) :-
	path(FirstNode, OneButLast, Path),
	s(OneButLast, LastNode),
	not(member(LastNode, Path)).

depth_first_iterative_deepening(Node, Solution) :-
	path(Node, GoalNode, Solution),
	goal(GoalNode).

part1(Total) :-
    writeln('Advent of Code 2025, Day 11, Part 1:'),
    setof(S, solve(you, S), SL1),
    length(SL1, Total),
    format('Total number of paths = ~w\n', [Total]).

part2(Total) :-
    writeln('Advent of Code 2025, Day 11, Part 2:'),
    setof(S, (solve(svr, S),member(dac, S)), SL1),
    findall(S2, (member(S2, SL1), member(fft, S)), SL2),
    length(SL2, Total),
    format('Total number of paths = ~w\n', [Total]).
