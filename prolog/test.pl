:- module(test,[]).
:- use_module(term_expander,[enable_write_out]).

:- enable_write_out.

foo(bar(a,b)).
