:- module(tmp,[]).
:- use_module(annotations,[spec_pre/2, spec_post/3, declare_spec/1, define_spec/2]).
:- use_module(library(avl)).


foo(a).

car(b).

bar(X,Y) :-
    (foo(X), car(Y); foo(Y),car(X)).
