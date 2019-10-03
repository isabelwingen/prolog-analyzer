:- module(tmp,[]).
:- use_module('../prolog/annotations',[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).


foo(X, Y) :- atom(X), bar(X, Y).
:- spec_pre(bar/2, [int, atom]).
:- spec_pre(bar/2, [atom, int]).
bar(3, a).
bar(a, 3).
