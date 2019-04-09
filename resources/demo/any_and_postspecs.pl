:- module(any_and_postspecs,[]).
:- use_module("../prolog/annotations.pl",[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).



%:- spec_pre(int/1,[list(atom)]).
%:- spec_pre(int/1,[atom]).
%:- spec_pre(int/1, [one_of([list(atom), atom])]).
:- spec_pre(int/1, [any]).
%:- spec_post(int/1,[list(atom)],[list(atom)]).
%:- spec_post(int/1,[atom],[atom]).
%int([]).
%int([H|T]) :-
%    int(H),
%    int(T).
%int(Statement) :-
%    atom(Statement).

:- spec_pre(foo/1, [one_of([int, atom])]).
:- spec_post(foo/1, [int], [int]).
:- spec_post(foo/1, [atom], [atom]).
foo(_).

%:- spec_pre(bar/1, [one_of([int, atom])]).
:- spec_pre(bar/1, [nonvar]).
bar(X) :-
    foo(X).
