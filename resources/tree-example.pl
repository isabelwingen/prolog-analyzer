:- module(tree_example,[]).
:- use_module("../prolog/prolog_analyzer",[enable_write_out/0,spec_pre/2,spec_post/3,spec_invariant/2,define_spec/2,declare_spec/1]).

:- enable_write_out.

:- declare_spec(tree(any(X))).
:- define_spec(tree(any(X)),one_of([compound(node(tree(any(X)),any(X),tree(any(X)))),atom(empty)])).

get_root(tree(_,Root,_),Root).

get_left_tree(tree(L,_,_),L).
get_right_tree(tree(_,_,R),R).



:- spec_pre(tree_search/3,[tree(int),int,var]).
:- spec_post(tree_search/3,[tree(int),int,var],[tree(int),int,list(int)]).
tree_search(T,Elem,Path) :-
    tree_search(T,Elem,[],Path).

:- spec_pre(tree_search/4,[tree(int),int,list(int),var]).
:- spec_post(tree_search/4,[tree(int),int,list(int),var],[tree(int),int,list(int),list(int)]).
tree_search(node(_,Elem,_),Elem,In,[Elem|In]) :- !.
tree_search(node(L,Root,_),Elem,In,Res) :-
    Elem < Root,!,
    tree_search(L,Elem,[Root|In],Res).
tree_search(node(_,Root,R),Elem,In,Res) :-
    tree_search(R,Elem,[Root|In],Res).



:- spec_pre(add_to_tree/3,[int,tree(int),var]).
:- spec_pre(add_to_tree/3,[int,tree(int),tree(int)]).
:- spec_post(add_to_tree/3,[int,tree(int),var],[int,tree(int),tree(int)]).
:- spec_post(add_to_tree/3,[int,tree(int),tree(int)],[int,tree(int),tree(int)]).
add_to_tree(Elem,empty,node(empty,Elem,empty)) :- !.
add_to_tree(Elem,node(empty,Root,Right),node(node(empty,Elem,empty),Root,Right)) :-
    Elem < Root, !.
add_to_tree(Elem,node(Left,Root,empty),node(Left,Root,node(empty,Elem,empty))) :-
    Elem > Root, !.

add_to_tree(Elem,node(Left,Root,Right),node(NLeft,Root,Right)) :-
    Elem < Root,
    add_to_tree(Elem,Left,NLeft).
add_to_tree(Elem,node(Left,Root,Right),node(Left,Root,NRight)) :-
    Elem > Root,
    add_to_tree(Elem,Right,NRight).
