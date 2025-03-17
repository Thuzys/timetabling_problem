:- module(load_names, [load_names/1]).
:- use_module(load).

% A predicate to load the names file
load_names(File) :-
    atomic_list_concat([File, '.names'], Path),
    load_file(Path, Lines),
    assert_names(Lines).

% A predicate to assert the names as facts
assert_names([]).
assert_names([H | T]) :-
    assert_names([H | T], 1).

assert_names([], _).
assert_names([H | T], Idx) :-
    assert_name(H, Idx),
    NewIdx is Idx + 1,
    assert_names(T, NewIdx).

% A predicate to assert a name as a fact
assert_name([Name], Idx) :-
    assertz(course_name(Idx, Name)).