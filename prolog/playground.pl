:- module(tmp,[]).

%foo2(R,H) :-
%    var(R),
%    [H|R] = [1,2,a].

%foo3(H,R) :-
%    var(R),
%    [H|R] = [1,2,a].


foo4(L) :-
    L = [H|T],
    T = [1,2,3],
    H = a.
