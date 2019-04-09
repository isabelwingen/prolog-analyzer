:- module(module2,[bar/2]).
:- use_module("../prolog/annotations",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).


bar(a,b).
