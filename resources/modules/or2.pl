:- module(or2, []).
:- use_module(or1, [bla/1]).

foo(X) :-
    (bla(X); blubb(X)).

:- spec_pre(blubb/1, [float]).
blubb(_).
