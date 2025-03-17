:- module(load_conflict, [load_conflict/1]).
:- use_module(load).

% A predicate to load the conflict file
load_conflict(File) :-
    atomic_list_concat([File, '.cm'], Path),
    load_file(Path, Lines),
    assert_facts(Lines).

% A predicate to assert each line as a fact
assert_facts([]).
assert_facts([H | T]) :-
    assert_facts([H | T], 1).

assert_facts([], _).
assert_facts([H | T], Idx) :-
    assert_line(H, Idx),
    NewIdx is Idx + 1,
    assert_facts(T, NewIdx).

% A predicate to assert each element of a line as a fact
assert_line([], _).
assert_line([H | T], Idx) :-
    assert_line([H | T], Idx, 1).

assert_line([], _, _).
assert_line([H | T], Idx, Idy) :-
    assertz(conflict(Idx, Idy, H)),
    NewIdx is Idy + 1,
    assert_line(T, Idx, NewIdx).