:- module(tmp,[]).

spec_post(_,_,_).

:- spec_post(foo/3, [0:int,1:any], [[2:atom,1:atom],[2:float,1:list(int)]]).

foo(_,_,_).

foo(L) :-
    L = '.'(A,B).

goo(X) :-
    atom(X).
