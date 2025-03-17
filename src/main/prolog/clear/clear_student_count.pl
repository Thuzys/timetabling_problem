:- module(clear_student_count, [clear_student_count/0]).
:- use_module('../load/load_student_count').

% Clear all facts 'student_count'
clear_student_count :-
    retractall(load_student_count:student_count(_, _)).