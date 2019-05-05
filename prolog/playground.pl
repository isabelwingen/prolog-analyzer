:- module(tmp,[]).
:- use_module(annotations,[spec_pre/2, spec_post/3, declare_spec/1, define_spec/2]).
:- use_module(library(avl)).

:- declare_spec(avl_tree(specvar(_Key), specvar(_Value))).
:- define_spec(avl_tree(specvar(Key), specvar(Value)), one_of([compound(node(specvar(Key),
                                                                             specvar(Value),
                                                                             integer,
                                                                             avl_tree(specvar(Key),specvar(Value)),
                                                                             avl_tree(specvar(Key),specvar(Value)))),
                                                               atom(empty)])).

:- declare_spec(tree(specvar(_))).
:- define_spec(tree(specvar(X)),one_of([compound(node(tree(specvar(X)),specvar(X),tree(specvar(X)))),atom(empty)])).


:- declare_spec(bla(specvar(_))).
:- define_spec(bla(specvar(X)),compound(node(atom(empty),specvar(X),atom(empty)))).


:- spec_pre(root/2,[var,nonvar]).
:- spec_pre(root/2,[tree(any),var]).
:- spec_pre(root/2,[tree(any),nonvar]).
:- spec_post(root/2,[any,any],[tree(union(X)),union(X)]).
root(node(_,R,_),R).

foo(B) :-
    root(node(empty,1,empty),B).
