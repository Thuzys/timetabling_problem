:- module(clear_conflict, [clear_conflict/0]).
:- use_module('../load/load_conflict').

% Clear all facts 'conflict'
clear_conflict :-
    retractall(load_conflict:conflict(_, _, _)).