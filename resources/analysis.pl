:- module(analysis,[]).
:- use_module("../prolog/prolog_analyzer",[enable_write_out/0,spec_pre/2,spec_post/3,spec_invariant/2,define_spec/2,declare_spec/1]).

:- enable_write_out.


foo(X,Y) :-
    X = 1,
    Y = 2,
    bar(X,Y),
    int(X).

:- spec_pre(bar/2,[][int,int]).
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

:- spec_pre(is_var/1,[var]).
is_var(X) :-
    var(X).

test_is_var(A) :-
    int(A),
    is_var(A).
