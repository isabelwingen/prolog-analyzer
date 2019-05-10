:- module(global,[]).

foo(X) :-
    X = [1,2,3].


bar(A,T,X) :-
    foo([A|T]),
    [A] = [A|X].


xyz(B,C,X) :-
    bar(B,C,X).
