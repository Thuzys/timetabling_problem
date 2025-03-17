:- module(timetabler, [solve_1Epoch/3, solve_2Epoch/3]).

first_epoch_periods(18).
second_epoch_periods(12).
total_students(1238).

:- use_module(library(clpfd)).
:- use_module(library(random)).

:- use_module('constraints/H1').
:- use_module('constraints/H2').
:- use_module('constraints/S1').
:- use_module('load/load_conflict').
:- use_module('load/load_student_count').
:- use_module('load/load_names').
:- use_module('print/print_schedule').
:- use_module('clear/clear_all').

:- dynamic exam/2.
:- dynamic best_cost/2.
:- dynamic best_cost/3.

% Only Time is required, in seconds
solve_1Epoch(Schedule, Cost, Time) :-
    retractall(exam(_, _)),
    retractall(best_cost(_, _)),
    assert(best_cost(inf, [])),
    clear_all,

    load_conflict('../resources/deetc0910_1stEpoch'),
    load_student_count('../resources/deetc0910_1stEpoch'),
    load_names('../resources/deetc0910_1stEpoch'),

    exam_count(N),
    first_epoch_periods(T),
    total_students(M),

    generate1Epoch(N, T, M, Schedule, Cost, Time).

% Only Time is required, in seconds
solve_2Epoch(Schedule, Cost, Time) :-
    retractall(exam(_, _)),
    retractall(best_cost(_, _, _)),
    assert(best_cost(inf, inf, [])),
    clear_all,

    load_conflict('../resources/deetc0910_2ndEpoch'),
    load_student_count('../resources/deetc0910_2ndEpoch'),
    load_names('../resources/deetc0910_2ndEpoch'),

    exam_count(N),
    first_epoch_periods(T1),
    second_epoch_periods(T2),
    total_students(M),

    generate2Epoch(N, T1, T2, M, Schedule, Cost, Time).

% Only Time is required, in seconds
generate2Epoch(N, T1, T2, M, Schedule, Cost, Time) :-
    length(Schedule1, N),
    Schedule1 ins 1..T1,
    length(Schedule2, N),
    LowerBound is T1 + 1,
    UpperBound is T1 + T2,
    Schedule2 ins LowerBound..UpperBound,
    constraints_H1:enforce_no_conflicts_same_period(Schedule1),
    constraints_H1:enforce_no_conflicts_same_period(Schedule2),
    constraints_H2:enforce_min_days_between_exams(Schedule1, Schedule2),
    append(Schedule1, Schedule2, Schedule),
    T is T1 + T2,
    catch(call_with_time_limit(Time, minimize_cost_2(Schedule1, Schedule2, Schedule, M)), time_limit_exceeded, handle_time_limit_2(Schedule, N, Cost, T)), !.

% Only Time is required
generate1Epoch(N, T, M, Schedule, Cost, Time) :-
    length(Schedule, N),
    Schedule ins 1..T,
    constraints_H1:enforce_no_conflicts_same_period(Schedule),
    catch(call_with_time_limit(Time, minimize_cost(Schedule, M, Cost)), time_limit_exceeded, handle_time_limit(Schedule, N, Cost, T)), !.

handle_time_limit(Schedule, N, Cost, T) :-
    best_cost(Cost, Schedule),
    assert_exams(Schedule, 0, N),
    print_schedule:write_csv_tables('deetc0910_1Epoch', T).

handle_time_limit_2(Schedule, N, Cost, T) :-
    best_cost(Cost1, Cost2, Schedule),
    assert_exams(Schedule, 0, N),
    Cost is Cost1 + Cost2,
    print_schedule:write_csv_tables('deetc0910_2Epoch', T).

minimize_cost(Schedule, M, Cost) :-
    random(0, 1000000, Seed),
    labeling([ff, random_value(Seed)], Schedule),
    constraints_S1:proximity_cost(Schedule, M, Cost),
    update_best_cost(Cost, Schedule),
    fail.

minimize_cost_2(Schedule1, Schedule2, Schedule, M) :-
    random(0, 1000000, Seed),
    labeling([ff, random_value(Seed)], Schedule),
    constraints_S1:proximity_cost(Schedule1, M, Cost1),
    constraints_S1:proximity_cost(Schedule2, M, Cost2),
    update_best_cost_2(Cost1, Cost2, Schedule),
    fail.

update_best_cost(Cost, Schedule) :-
    best_cost(CurrentBestCost, _),
    (Cost < CurrentBestCost ->
        retractall(best_cost(_, _)),
        assert(best_cost(Cost, Schedule))
    ; true).

update_best_cost_2(Cost1, Cost2, Schedule) :-
    best_cost(CurrentBestCost1, CurrentBestCost2, _),
    (Cost1 < CurrentBestCost1, Cost2 < CurrentBestCost2 ->
        retractall(best_cost(_, _, _)),
        assert(best_cost(Cost1, Cost2, Schedule))
    ; true).
    
exam_count(ExamCount) :-
    findall(C, load_student_count:student_count(_, C), Courses),
    length(Courses, ExamCount).

assert_exams([], _ , _).
assert_exams([H | T], Index, ExamCount) :-
    ExamIndex is Index mod ExamCount + 1,
    load_names:course_name(ExamIndex, Course),
    assert(exam(Course, H)),
    NextIndex is Index + 1,
    assert_exams(T, NextIndex, ExamCount).

print_variable_domain(Var) :-
    fd_dom(Var, Dom),
    write(Var), write(' domain: ['), write(Dom), write(']'), nl.