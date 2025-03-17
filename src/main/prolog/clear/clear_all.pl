:- module(clear_all, [clear_all/0]).

:- use_module(clear_conflict).
:- use_module(clear_names).
:- use_module(clear_student_count).

% Clear all facts
clear_all :-
    clear_conflict,
    clear_names,
    clear_student_count.