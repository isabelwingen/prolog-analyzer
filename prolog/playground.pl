:- module(tmp,[]).
:- use_module(prolog_analyzer,[spec_pre/2, spec_post/3, spec_invariant/2, enable_write_out/0, declare_spec/1, define_spec/2]).

:- enable_write_out.
:- declare_spec(tree(specvar(_))).
:- define_spec(tree(specvar(X)),one_of([compound(node(tree(specvar(X)),specvar(X),tree(specvar(X)))),atom(empty)])).


:- spec_pre(tree_search/3,[tree(specvar(X)),specvar(X),var]).
tree_search(T,Elem,Path) :-
    tree_search(T,Elem,[],Path).

:- spec_pre(tree_search/4,[tree(specvar(X)),specvar(X),list(specvar(X)),var]).
tree_search(node(_,Elem,_),Elem,In,[Elem|In]) :- !.
tree_search(node(L,Root,_),Elem,In,Res) :-
    Elem < Root,!,
    tree_search(L,Elem,[Root|In],Res).
tree_search(node(_,Root,R),Elem,In,Res) :-
    tree_search(R,Elem,[Root|In],Res).
