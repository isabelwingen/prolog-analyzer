:- module(test,[]).
:- use_module("../prolog/annotations",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- spec_pre(foo/2,[number]).
foo(X) :-
    member(Y,[1,2,3]).
