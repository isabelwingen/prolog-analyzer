:- module(tmp,[]).
:- use_module('../prolog/annotations',[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

foo(X) :-
    (\+ int(X) -> true ; true).

goo(X) :-
    (int(X) -> true ; true).

tree(empty).
tree(node(L,empty,R)) :-
    tree(L),
    tree(R).

:- spec_pre(tree/1, [any]).
:- spec_post(tree/1, [0:any], [[0:any]]).

:- declare_spec(a).
:- define_spec(a, atom).

:- huhu.
