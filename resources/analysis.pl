:- module(analysis,[]).
:- use_module("../prolog/annotations",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

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
