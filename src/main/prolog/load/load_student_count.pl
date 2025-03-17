:- module(load_student_count, [load_student_count/1]).
:- use_module(load).

load_student_count(File) :-
    atomic_list_concat([File, '.crs'], Path),
    load_file(Path, Lines),
    assert_counts(Lines).

assert_counts([]).
assert_counts([[H, Num | _] | T]) :-
    assertz(student_count(H, Num)),
    assert_counts(T).