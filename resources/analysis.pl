:- module(analysis,[]).
:- use_module("../prolog/prolog_analyzer",[enable_write_out/0,spec_pre/2,spec_post/3,spec_invariant/2,define_spec/2,declare_spec/1]).

:- enable_write_out.


foo(X,Y) :-
    bar(X,Y),
    int(X).

:- spec_pre(bar/2,[int,int]).
:- spec_pre(bar/2,[atom,atom]).
bar(X,X).

:- spec_pre(int/1,[int]).
int(X) :-
    integer(X).


:- spec_pre(filter/2,[list(atomic),var]).
filter([X|L],X) :-
    int(X), !.
filter([_|L],X) :-
    filter(L,X).
