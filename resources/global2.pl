:- module(global2,[]).

foo(X,a,Y) :-
    Y = 3,!.

foo(X,_,Y) :-
    Y = atom.


bla(Y) :-
    foo(X,3,Y).
