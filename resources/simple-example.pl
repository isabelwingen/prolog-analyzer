:- module(simple_example,[]).
:- use_module("  /prolog/annotations",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- spec_pre(mmember/2,[int,list(int)]).
:- spec_pre(mmember/2,[var,list(int)]).
mmember(H,[H|_]).
mmember(E,[_|T]) :-
    mmember(E,T).
