:- module(tree_example,[]).
:- use_module("../prolog/annotations",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- declare_spec(a).
:- define_spec(a,atom).

:- declare_spec(tree(specvar(_))).
:- define_spec(tree(specvar(X)),one_of([compound(node(tree(specvar(X)),specvar(X),tree(specvar(X)))),atom(empty)])).

:- spec_pre(get_root/2,[tree(int),int]).
get_root(node(_,Root,_),Root).

get_left_tree(node(L,_,_),L).
get_right_tree(node(_,_,R),R).
