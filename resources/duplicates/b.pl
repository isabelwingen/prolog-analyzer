:- module(b,[]).
:- use_module("c.pl",[days/1]).
:- use_module(e).
:- use_module(library(lists),[reverse/2]).

foo(L,A) :-
    days(L),
    yo(A),
    reverse([1,2],K).

days("hallo").
