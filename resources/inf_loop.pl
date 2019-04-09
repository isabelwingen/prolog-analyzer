:- module(inf_looo,[]).
:- use_module("../prolog/annotations",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- declare_spec(op).
:- declare_spec(expr).
:- declare_spec(cst).

:- define_spec(op, one_of([atom(+), atom(-)])).
:- define_spec(expr, one_of([cst, compound(expr(op,expr,expr)), compound(neg(expr))])).
:- define_spec(cst,compound(cst(int))).

:- spec_pre(int_expr/1,[expr]).
:- spec_post(int_expr/1,[expr],[ground]).
int_expr(expr(Op,A,B)) :-
    int_expr(A).
