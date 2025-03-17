:- module(constraints_S1, [proximity_cost/3]).
:- use_module(library(clpfd)).
:- use_module('../load/load_conflict').

proximity_cost(Schedule, M, Cost) :-
    length(Schedule, N),
    sum_cost(Schedule, N, 0, CostSum),
    Cost #= CostSum div M.

sum_cost([], _, Acc, Acc). % Base case: No more exams, return accumulated cost.
sum_cost([Ti | Rest], N, Acc, Cost) :-
    sum_cost_pair(Ti, Rest, 1, N, RestCost),
    NewAcc #= Acc + RestCost,
    sum_cost(Rest, N, NewAcc, Cost).

sum_cost_pair(_, [], _, _, 0).  % No more exams to compare to
sum_cost_pair(Ti, [Tj | Rest], I, N, Acc) :-
    I < N, 
    J is I + 1,  % Correct the index for the second exam
    load_conflict:conflict(I, J, Cij),
    ( Cij #> 0, abs(Ti - Tj) #=< 5, abs(Ti - Tj) #> 0 -> 
        ProximityPenalty #= 2 ^ (5 - abs(Ti - Tj)),
        CostIJ #= Cij * ProximityPenalty
    ; CostIJ #= 0
    ),
    NextI is I + 1,
    sum_cost_pair(Ti, Rest, NextI, N, RestCost),
    Acc #= CostIJ + RestCost.