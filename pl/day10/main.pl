/* -*- prolog-mode -*- */
% Advent of Code 2025 - Day 10: Factory

:- use_module(library(clpfd)).

:- dynamic started/1.
:- dynamic finished/2.

% Read the input file.

read_input_line(Stream, String) :- 
	read_line_to_string(Stream, String),
	(
	  String = ""
	  ;
	  String \== end_of_file
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

% DCG for input file.

digit(D) --> [D],
	{	code_type(D, digit)
	}.

digits([D | Tail]) --> digit(D), !, digits(Tail).
digits([]) --> [].

integer(F) --> digits(Digits),
	{	number_codes(F, Digits)
	}.

state(0) --> ".".
state(1) --> "#".

states([S | Tail]) --> state(S), !, states(Tail).
states([]) --> [].

int_list([F | Tail], Separator) --> integer(F), Separator, int_list(Tail, Separator).
int_list([F], _) --> integer(F).

indicator_spec(ISL) --> "[", states(ISL), "]".
button_spec(BSL) --> "(", int_list(BSL, ","), ")".
joltage_spec(JSL) --> "{", int_list(JSL, ","), "}".

button_spec_list([BS | Tail]) --> button_spec(BS), " ", button_spec_list(Tail).
button_spec_list([BS]) --> button_spec(BS).

machine_spec([ISL, BSL, JSL]) --> indicator_spec(ISL), " ", button_spec_list(BSL), " ", joltage_spec(JSL).

% Decode a line of input using DCG rules.
process_line(Line, MS) :-
    string_codes(Line, Chars),
    phrase(machine_spec(MS), Chars, []).

% Utilities.

% Generate a vector of repeat values gen_repeat_values(Value, Count, List).
gen_repeat_values(_, 0, []) :- !.
gen_repeat_values(Value, Count, [Value | Rest]) :-
	Count2 is Count - 1,	
	gen_repeat_values(Value, Count2, Rest).

% set_item(Index, Before, Item, After)
set_item(1, [_ | Tail], Item, [Item | Tail]).
set_item(N, [Head | Tail], Item, [Head | Tail2]) :-
	N > 1,
	M is N - 1,
	set_item(M, Tail, Item, Tail2).

% gen_range(Low, High, Values)
gen_range(Low, High, []) :- 
    Low > High, !.
gen_range(Low, High, [Low | Tail]) :-
    Low2 is Low + 1,
    gen_range(Low2, High, Tail).

% select_column(Matrix, ColNum, Column)
select_column([], _, []).
select_column([Row | MTail], ColNum, [Element | CTail]) :-
    nth1(ColNum, Row, Element),
    select_column(MTail, ColNum, CTail).

% vector_product_sortof(Vector1, Vector2, Product)
% Product vector contains products of corresponding elements from other vectors.
vector_product_sortof([], [], []).
vector_product_sortof([V1 | VTail], [W1 | WTail], [P1 | PTail]) :-
    P1 #= V1 * W1,
    vector_product_sortof(VTail, WTail, PTail).

% bounded_sum(List, Total).
% Elements in the given list must sum to the given Total.
bounded_sum([], 0).
bounded_sum([V1 | Tail], Total) :-
    bounded_sum(Tail, TailSum),
    Total #= TailSum + V1.

% Solution to problem.

% Converting 0-based index specification of button connections into a template of which
% indicators are affected by a button press.
convert_spec([], Before, Before).
convert_spec([Index | Tail], Before, After) :-
    J is Index + 1,
    set_item(J, Before, 1, InProcess1),
    convert_spec(Tail, InProcess1, After).

convert_spec_to_template(Spec, Template, NumIndicators) :-
    gen_repeat_values(0, NumIndicators, BT1),
    convert_spec(Spec, BT1, Template).

% convert_button_specs(ButtonSpecs, ButtonTemplates, NumIndicators) :-
convert_button_specs([], [], _).
convert_button_specs([ButtonSpec | SpecTail], [ButtonTemplate | TemplateTail], NumIndicators) :-
    convert_spec_to_template(ButtonSpec, ButtonTemplate, NumIndicators),
    convert_button_specs(SpecTail, TemplateTail, NumIndicators).

% Use template and specified total Joltage to bound possible multiple of template from above.
bound_multiple([], _, []).
bound_multiple([0 | TTail], Multiple, [_ | JTail]) :-
    !, bound_multiple(TTail, Multiple, JTail).
bound_multiple([1 | TTail], Multiple, [Joltage | JTail]) :-
    Multiple in 0..Joltage,
    bound_multiple(TTail, Multiple, JTail).

% Set up bounded multiples of given templates, based on given Joltages that must not be exceeded.
setup_multiples([], [], _).
setup_multiples([Template | TTail], [Multiple | MTail], Joltages) :-
    Multiple #>= 0,
    bound_multiple(Template, Multiple, Joltages),
    setup_multiples(TTail, MTail, Joltages).

% In: Templates, Multiples, Joltages.
% Sum across templates of TV * MV in corresponding positions = Corresponding joltage.

% Pick out each indicator by index in templates and joltages lists, then constrain so that products
% of multiples and template values sum to the corresponding joltage for a given indicator.
constrain_indexed_columns([], _, _, _).
constrain_indexed_columns([Index | ITail], Templates, Multiples, Joltages) :-
    select_column(Templates, Index, Vector1),
    nth1(Index, Joltages, JV1),
    scalar_product(Vector1, Multiples, #=, JV1),
    % vector_product_sortof(Vector1, Multiples, Vector2),
    % bounded_sum(Vector2, JV1),
    constrain_indexed_columns(ITail, Templates, Multiples, Joltages).

negate_template([], []).
negate_template([P | T1], [N | T2]) :-
    N is P * -1,
    negate_template(T1, T2).

vector_sum([], [], []).
vector_sum([A | ATail], [B | BTail], [C | CTail]) :-
    C is A + B,
    vector_sum(ATail, BTail, CTail).

% Pick out distinct pairs of indicators by index in templates and joltages lists,
% then constrain so that products
% of multiples and template values sum to the corresponding joltage for a given indicator.
constrain_different_columns([], _, _, _).
constrain_different_columns([Index1 | ITail], Templates, Multiples, Joltages) :-
    % Grab first column.
    select_column(Templates, Index1, Vector1),
    nth1(Index1, Joltages, JV1),

    findall([DJ4,Vector4], (
        member(Index2, ITail),
        select_column(Templates, Index2, Vector2),
        nth1(Index2, Joltages, JV2),
        % Difference the two columns and JVs.
        DJ4 is JV1 - JV2,
        negate_template(Vector2, Vector3),
        vector_sum(Vector1, Vector3, Vector4)
    ), Equations),

    % Grab another column to compare with first one.
    findall(DJ5, (
        member([DJ5,Vector5], Equations),
        scalar_product(Vector5, Multiples, #=, DJ5)),
    _),
    % foreach(member([DJ, Vector5], Equations),
    %         % Constrain the Multiples according to the difference.
    %         scalar_product(Vector5, Multiples, #=, DJ)),

    % vector_product_sortof(Vector1, Multiples, Vector2),
    % bounded_sum(Vector2, JV1),
    constrain_different_columns(ITail, Templates, Multiples, Joltages).


% Further constrain Multiples so that they satisfy the required total joltage for each output.
constrain_joltage_sums(Templates, Multiples, Joltages) :-
    length(Joltages, NumJoltages),
    gen_range(1, NumJoltages, Indices),
    constrain_indexed_columns(Indices, Templates, Multiples, Joltages),
    constrain_different_columns(Indices, Templates, Multiples, Joltages).

% Solve the given line in input file.
solve_line(Line, Multiples, NumPresses) :-
    process_line(Line, [Indicators, ButtonSpecs, Joltages]),
    length(Indicators, NumIndicators),
    convert_button_specs(ButtonSpecs, ButtonTemplates, NumIndicators),
    setup_multiples(ButtonTemplates, Multiples, Joltages),
    constrain_joltage_sums(ButtonTemplates, Multiples, Joltages),
    max_list(Joltages, MaxJoltage),
    sum(Multiples, #=, NumPresses),
    NumPresses #>= MaxJoltage.

num_presses_required(Line, NP) :-
    solve_line(Line, Multiples, NP),
    % sum(Multiples, #=, NP),
    % bounded_sum(Multiples, NP),

    % Find solution with minimal NP, using first fail strategy.
    % - label var with smallest domain and participating in most constraints next. 
    append(Multiples, [NP], Vars),
    labeling([ff,min(NP)],Vars),!.
    % labeling([ffc,min(NP)],[NP]),!.
    % fd_inf(NP, MinNP).

conc_npr(Lines, CountList) :-
    retractall(started),
    retractall(finished),
    concurrent_forall(member(Line,Lines),(
        format("Starting: ~w\n", [Line]),
        assertz(started(Line)),
        num_presses_required(Line,NP),
        format("Finished: ~w <-- ~w\n", [NP,Line]),
        assertz(finished(Line,NP))
    )),
    findall(N1, finished(_,N1), CountList),
    writeln('All Done.').

% 286 too low.

part2(FileName, Total) :-
    writeln('Advent of Code 2025, Day 10, Part 2:'),
    read_input_data(FileName, Lines),
    conc_npr(Lines, NPL),
%     findall(NP, (member(Line,Lines),num_presses_required(Line,NP),format('~w : ~w\n',[NP,Line])), NPL),
    sum(NPL, #=, Total),
    format('Total number of button presses required for file ~w = ~w\n', [FileName, Total]).
