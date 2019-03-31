:- module(tmp,[]).
:- use_module(prolog_analyzer,[spec_pre/2, spec_post/3, enable_write_out/0]).

:- enable_write_out.

:- spec_pre(foo/1, [tuple([list(atom)])]).
foo([[a,b,c]]).
