:- module(constraints_H2, [enforce_min_days_between_exams/2]).
:- use_module(library(clpfd)).

enforce_min_days_between_exams([], []).
enforce_min_days_between_exams([H1 | Rest1], [H2 | Rest2]) :-
    abs(H1 - H2) #> 5,
    enforce_min_days_between_exams(Rest1, Rest2).