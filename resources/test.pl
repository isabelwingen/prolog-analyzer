:- module(tmp,[]).
:- use_module('../prolog/annotations',[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).
:- use_module(library(dcg/basics)).

foo(a).
foo(c(X)) :-
    foo(X).

is_empty_set(b(ES,_,_)) :- is_empty_set_aux(ES).
is_empty_set_aux(empty_set).
is_empty_set_aux(empty_sequence).
is_empty_set_aux(domain(D)) :- is_empty_set(D).
is_empty_set_aux(range(D)) :- is_empty_set(D).


bla(X) :-
    is_empty_set(X).


foo(X, Y) :-
    int2(X),
    atom2(X),
    int2(Y),
    atom2(Y).

g(A) :-
    int2(A),
    atom2(A).

:- spec_pre(int2/1, [int]).
:- spec_pre(atom2/1, [atom]).

int2(_).
atom2(_).
