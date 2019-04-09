:- module(simple_example,[]).
:- use_module("../prolog/annotations",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- spec_pre(member/2,[int,list(int)]).
:- spec_pre(member/2,[var,list(int)]).
:- spec_post(member/2,[var,list(int)],[int,list(int)]).
member(H,[H|_]).
member(E,[_|T]) :-
    member(E,T).
