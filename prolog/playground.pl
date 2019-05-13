:- module(tmp,[]).


foo1(V,H,T) :- var(T),!,V = [H|T].

foo2(V,H,T) :-
    var(T),
    [H|T] = [1,2,3,4].
