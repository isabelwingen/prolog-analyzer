:- module(module2,[bar/2]).
:- use_module("../prolog/prolog_analyzer",[spec_pre/2,spec_post/3,enable_write_out/0,declare_spec/1,define_spec/2]).

:- enable_write_out.

bar(a,b).

