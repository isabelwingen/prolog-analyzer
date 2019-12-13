:- module(b,[]).
:- use_module("c.pl",[bla/2]).
:- use_module(e).
:- use_module(library(lists)).

foo(L,A) :-
    days(L),
    yo(A).

days(3.6).

yoyo(L) :-
    bla(L,L).
