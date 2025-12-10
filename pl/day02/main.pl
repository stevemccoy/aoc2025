/* -*- prolog-mode -*- */
% Advent of Code 2025 - Day 2: Gift Shop

:- use_module(library(clpfd)).

% Read the input file.

read_input_line(Stream, String) :- 
	read_line_to_string(Stream, String),
	(
	  String = ""
	  ;
	  String \== end_of_file,
  	  assertz(required(String))
    ).

read_input_lines(Stream, [String | Tail]) :-
	read_input_line(Stream, String),
	!,
	read_input_lines(Stream, Tail).
read_input_lines(_, []).

read_input_data(FileName, Lines) :-
	open(FileName, read, Stream),
	read_input_lines(Stream, Lines),
	!,
	close(Stream).

% Aggregate elements of a list successively by a named function(X,Y,Z), producing a scalar result.
my_aggregate_list([A], _, A).
my_aggregate_list([X, Y | InTail], Function, Result) :-
	Goal =.. [Function, X, Y, Z],
	Goal,
	my_aggregate_list([Z | InTail], Function, Result).

% Map adjacent pairs of list items onto the result of applying named function(X,Y,Z) to them.
my_map_adjacent_pairs([_], _, []).
my_map_adjacent_pairs([X, Y | InTail], Function, [Z | OutTail]) :-
	Goal =.. [Function, X, Y, Z],
	Goal,
	my_map_adjacent_pairs([Y | InTail], Function, OutTail).

mysum(X, Y, Z) :-
	Z is X + Y.

maxlist(L, M) :-
	my_aggregate_list(L, max, M).

sumlist(L, S) :-
	my_aggregate_list(L, mysum, S).

% Day 2 specific solution.

ceil10(0,1).
ceil10(1,10).
ceil10(2,100).
ceil10(3,1000).
ceil10(4,10000).
ceil10(5,100000).
ceil10(6,1000000).
ceil10(7,10000000).
ceil10(8,100000000).
ceil10(9,1000000000).
ceil10(10,10000000000).
ceil10(11,100000000000).
ceil10(12,1000000000000).

% Find the number of digits and upper bound on given value X.
find_ceiling(X, N, Ceiling) :-
    ceil10(N, Ceiling),
    Ceiling #> X,
    M #= N - 1,
    ceil10(M, Y),
    Y #=< X.

% Generate values X within given min and max range, where digits are
% a repetition of the same pattern twice.
value_in_range(Min, Max, X) :-
    X in Min..Max,
    find_ceiling(X, XN, _),
    XN #= HN * 2,
    ceil10(HN, Ceiling),
    HM #= HN - 1,
    ceil10(HM, Floor),
    UpperBound #= Ceiling - 1,
    H in Floor..UpperBound,
    G #= H * Ceiling,
    X #= G + H.

count_values_in_range(Min, Max, Count) :-
    setof(X, (value_in_range(Min, Max, X), labeling([min(X)],[X])), XL),
    !,
    length(XL, Count).
count_values_in_range(_, _, 0).

values_in_range(Min, Max, XL) :-
    setof(X, (value_in_range(Min, Max, X), labeling([min(X)],[X])), XL),
    !.
values_in_range(_, _, []).

part1(FileName, Strings) :-
    writeln('Advent of Code 2025, Day 2, Part 1:'),
    read_input_data(FileName, L1),
    findall(L2, (member(S1, L1), split_string(S1, ",", "", L2)), L3),
    flatten(L3, L4),
    findall(L5, (member(S2, L4), split_string(S2, "-", "", L5)), L6),
    findall(R1, (member([S3, S4], L6), number_string(Min1, S3), number_string(Max1, S4), R1 = [Min1,Max1]), L7),
    findall(L8, (member([Min2, Max2], L7), values_in_range(Min2, Max2, L8), format('[~w - ~w]: ~w\n', [Min2,Max2,L8])), CL1),
    flatten(CL1, CL2),
    sumlist(CL2, Total),
    format('Total of invalid IDs for file ~w = ~w\n', [FileName, Total]),
    Strings = CL2.

