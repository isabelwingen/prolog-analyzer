:- module(test,[]).
:- use_module("../prolog/prolog_analyzer",[enable_write_out/0,spec_pre/2]).

:- enable_write_out.

:- spec_pre(foo/2,[number]).
foo(X) :-
    member(Y,[1,2,3]).
