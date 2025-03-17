:- module(constraints_H1, [enforce_no_conflicts_same_period/1]).
:- use_module(library(clpfd)).
:- use_module('../load/load_conflict').

enforce_no_conflicts_same_period(Schedule) :-
    length(Schedule, N),
    N1 is N - 1,
    enforce_no_conflicts(Schedule, 1, N1).

enforce_no_conflicts(_, I, N) :- I > N, !.
enforce_no_conflicts(Schedule, I, N) :-  % main loop
    J1 is I + 1,
    enforce_no_conflicts_between_pairs(Schedule, I, J1, N), % nested loop
    I1 is I + 1,
    enforce_no_conflicts(Schedule, I1, N).

enforce_no_conflicts_between_pairs(_, _, J, N) :- J > N, !.
enforce_no_conflicts_between_pairs(Schedule, I, J, N) :-
    nth1(I, Schedule, Ti),
    nth1(J, Schedule, Tj),
    load_conflict:conflict(I, J, Cij),
    (Cij > 0 -> Ti #\= Tj ; true),
    J1 is J + 1,
    enforce_no_conflicts_between_pairs(Schedule, I, J1, N).