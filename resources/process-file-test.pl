:- module(process_file_test,[]).
:- use_module("../prolog/prolog_analyzer",[enable_write_out/0,spec_pre/2,spec_post/3,spec_invariant/2,define_spec/2,declare_spec/1]).

:- enable_write_out.


:- declare_spec(intOrVar).
:- declare_spec(foo).

:- define_spec(foo,compound(foo(int,int))).
:- define_spec(intOrVar,one_of([int,var])).

:- define_spec(a,and([int,atom])).
:- define_spec(b,tuple([int,var])).
:- define_spec(c,atom(empty)).


:- spec_pre(member_int/2,[int,list(int)]).
:- spec_pre(member_int/2,[var,list(int)]).
:- spec_post(member_int/2,[var,list(int)],[int,list(int)]).
:- spec_invariant(member_int/2,[any,ground]).
member_int(H,[H,_]) :- !.
member_int(E,[_,T]) :-
    member_int(E,T).


:- spec_pre(foo/3,[foo,intOrVar,intOrVar]).
:- spec_post(foo/3,[foo,intOrVar,intOrVar],[foo,int,int]).
:- spec_post(foo/3,[nonvar,int,int],[foo,int,int]).
decode_foo(foo(A,B),A,B).
