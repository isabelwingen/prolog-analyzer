:- module(simple_example,[]).
:- use_module('../prolog/annotations',[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

:- spec_pre(mmember/2,[int,list(int)]).
:- spec_pre(mmember/2,[var,list(int)]).
mmember(H,[H|_]).
mmember(E,[_|T]) :-
    mmember(E,T).


foo(X, Y) :-
    atom(X),
    bar(X, Y).

:- spec_pre(bar/2, [int, atom]).
:- spec_pre(bar/2, [atom, int]).
bar(3, a).
bar(a, 3),


:- spec_pre(foo/2, [any, any]).
:- spec_post(foo/2, [], [[0:int, 1:int]; [0:atom,1:atom]]).
foo(1,1).
foo(a,a).
