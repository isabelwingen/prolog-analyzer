:- module(any_and_postspecs,[]).
:- use_module("../prolog/annotations.pl",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).



:- spec_pre(foo/1, [one_of([int, atom])]).
:- spec_post(foo/1, [int], [int]).
:- spec_post(foo/1, [atom], [atom]).
foo(_).

:- spec_pre(bar/1, [nonvar]).
bar(X) :-
    foo(X).

bar2(X) :-
    foo(1).
