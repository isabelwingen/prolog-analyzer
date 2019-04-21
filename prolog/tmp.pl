:- module(tmp,[]).
:- use_module(prolog_analyzer,[enable_write_out/0,declare_spec/1,define_spec/2,spec_pre/2,spec_post/3,spec_invariant/2]).
:- enable_write_out.

foo(a,b).