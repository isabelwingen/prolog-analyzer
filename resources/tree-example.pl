:- module(tree_example,[]).
:- use_module('../prolog/annotations',[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- declare_spec(tree(specvar(_))).
:- define_spec(tree(specvar(X)),one_of([compound(node(tree(specvar(X)),specvar(X),tree(specvar(X)))),atom(empty)])).

get_root(node(_,Root,_),Root).

get_left_tree(node(L,_,_),L).
get_right_tree(node(_,_,R),R).



:- spec_pre(tree_search/3,[tree(specvar(X)),specvar(X),var]).
:- spec_post(tree_search/3,[tree(specvar(X)),specvar(X),var],[tree(specvar(X)),specvar(X),list(specvar(X))]).
tree_search(T,Elem,Path) :-
    tree_search(T,Elem,[],Path).

:- spec_pre(tree_search/4,[tree(specvar(X)),specvar(X),list(specvar(X)),var]).
:- spec_post(tree_search/4,[tree(specvar(X)),specvar(X),list(specvar(X)),var],[tree(specvar(X)),specvar(X),list(specvar(X)),list(specvar(X))]).
tree_search(node(_,Elem,_),Elem,In,[Elem|In]) :- !.
tree_search(node(L,Root,_),Elem,In,Res) :-
    Elem < Root,!,
    tree_search(L,Elem,[Root|In],Res).
tree_search(node(_,Root,R),Elem,In,Res) :-
    tree_search(R,Elem,[Root|In],Res).



:- spec_pre(add_to_tree/3,[specvar(X),tree(specvar(X)),var]).
:- spec_pre(add_to_tree/3,[specvar(X),tree(specvar(X)),tree(specvar(X))]).
:- spec_post(add_to_tree/3,[specvar(X),tree(specvar(X)),var],[specvar(X),tree(specvar(X)),tree(specvar(X))]).
:- spec_post(add_to_tree/3,[specvar(X),tree(specvar(X)),tree(specvar(X))],[specvar(X),tree(specvar(X)),tree(specvar(X))]).
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

:- spec_pre(foo/1, [tree(int)]).
foo(L) :-
    get_right_tree(L,A).
