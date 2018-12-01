:- module(simple_example,[]).
:- use_module("../prolog/prolog_analyzer",[enable_write_out/0,spec_pre/2,spec_post/3]).

:- enable_write_out.

:- spec_pre(member/2,[int,list(int)]).
:- spec_pre(member/2,[var,list(int)]).
:- spec_post(member/2,[var,list(int)],[int,list(int)]).
member(H,[H|_]).
member(E,[_|T]) :-
    member(E,T).


