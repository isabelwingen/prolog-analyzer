:- module(any_and_postspecs,[]).
:- use_module("../../prolog/prolog_analyzer",[spec_pre/2,spec_post/3,enable_write_out/0,declare_spec/1,define_spec/2]).

:- enable_write_out.



%:- spec_pre(int/1,[list(atom)]).
%:- spec_pre(int/1,[atom]).
:- spec_pre(int/1, [one_of([list(atom), atom])]).
:- spec_post(int/1,[list(atom)],[list(atom)]).
:- spec_post(int/1,[atom],[atom]).
int([]).
int([H|T]) :-
    int(H),
    int(T).
int(Statement) :-
    atom(Statement).
