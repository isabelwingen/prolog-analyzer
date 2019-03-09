:- module(new_system,[]).
:- use_module("../prolog/prolog_analyzer",[enable_write_out/0,spec_pre/2,spec_post/3,spec_invariant/2,define_spec/2,declare_spec/1]).

:- enable_write_out.

:- spec_pre(bar/1, [int]).
bar(_).
