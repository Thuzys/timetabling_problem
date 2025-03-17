:- module(clear_names, [clear_names/0]).
:- use_module('../load/load_names').

% Clear all facts 'course_name'
clear_names :-
    retractall(load_names:course_name(_, _)).