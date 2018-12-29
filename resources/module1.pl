:- module(module1,[foo/3]).
:- use_module("../prolog/prolog_analyzer",[spec_pre/2,spec_post/3,enable_write_out/0,declare_spec/1,define_spec/2]).
:- use_module(module2).

:- enable_write_out.

foo(X,Y,Z) :-
    module2:bar(X,Y),
    member(Z,[X,Y]),
    duplicate(Z,L).

duplicate(Z,[Z,Z]).

