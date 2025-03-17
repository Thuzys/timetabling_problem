:- module(load, [load_file/2]).
:- use_module(library(readutil)).

% A predicate to load a file into a list of lines
load_file(File, Lines) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

% A predicate to read lines from a stream
read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        split_string(Line, "\t", "", Atoms), % Split the line into atoms
        maplist(convert_element, Atoms, Elems), % Convert the atoms to numbers
        Lines = [Elems | Rest], % Append the numbers to the list of lines
        read_lines(Stream, Rest) % Recursively read the rest of the lines
    ; Lines = []
    ).

convert_element(Element, Value) :-
    (   number_string(Value, Element)
        ->  true
        ;
        Value = Element
    ).
