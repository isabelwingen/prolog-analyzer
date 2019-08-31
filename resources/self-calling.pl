:- module(self,[]).

%% foo(a(Y)) :-
%%     Y = x.
%% foo(b(Y)) :-
%%     Y = y.
%% foo(c(X)) :-
%%     foo(X).
%% ERROR with incomplete list --> investigate

foo(a).
foo(c(X)) :-
    foo(X).



tree(empty).
tree(node(L,empty,R)) :-
    tree(L),
    tree(R).
