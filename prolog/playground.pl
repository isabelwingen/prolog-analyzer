:- module(tmp,[]).
:- use_module(annotations,[spec_pre/2, spec_post/3, declare_spec/1, define_spec/2]).
:- use_module(library(avl)).


foo(a \/ b).
foo(c /\ d).
