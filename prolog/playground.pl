:- module(tmp,[]).
%:- use_module(prolog_analyzer,[spec_pre/2, spec_post/3, declare_spec/1, define_spec/2]).

%:- enable_write_out.
%:- declare_spec(tree(specvar(_))).
%:- define_spec(tree(specvar(X)),one_of([compound(node(tree(specvar(X)),specvar(X),tree(specvar(X)))),atom(empty)])).


:- spec_pre(foo/1,[var]).
foo(L) :-
    P = [_|L].
