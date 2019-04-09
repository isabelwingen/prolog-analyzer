:- module(tmp,[]).
:- use_module("../prolog/annotations.pl",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- spec_pre(atom_member/2, [var, list(atom)]).
:- spec_pre(atom_member/2, [atom, list(atom)]).
:- spec_post(atom_member/2, [var, list(atom)], [atom, list(atom)]).
atom_member(H,[H|_]) :- !.
atom_member(E,[_|T]) :-
    atom_member(E,T).

:- spec_pre(do_stuff/1, [var]).
:- spec_post(do_stuff/1, [var], [integer]).
do_stuff(X).

:- spec_pre(checker/1, [var]).
checker(X) :-
    do_stuff(X),
    atom_member(X, [a,b,c,d]).
