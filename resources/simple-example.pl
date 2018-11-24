:- module(simple_example,[]).
:- use_module("../prolog/term_expander",[enable_write_out]).

:- enable_write_out.

member(H,[H|_]).
member(E,[_|T]) :-
    member(E,T).
