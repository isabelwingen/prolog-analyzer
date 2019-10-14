:- module(tmp,[]).

foo(X) :-
    (a(X); f(X)).

goo(X) :-
    (a(X) -> f(X); g(X)).

:- spec_pre(a/1, [atom]).
:- spec_pre(f/1, [float]).
:- spec_pre(g/1, [int]).

a(_).
f(_).
g(_).
