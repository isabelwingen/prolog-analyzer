:- module(new_system,[]).
:- use_module("../prolog/annotations",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- spec_pre(bar/1, [int]).
bar(_).

:- spec_pre(int/1,[atomic]).
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
