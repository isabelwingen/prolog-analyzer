:- module(annotations, [spec_pre/2, spec_post/3, spec_invariant/2, define_spec/2, declare_spec/1]).
initialize_dialect :-
    prolog_load_context(dialect,swi),!,
    use_module(library(error)),
    use_module(library(apply)),
    use_module(library(lists)).

initialize_dialect :-
    use_module(library(file_systems)),
    use_module(library(lists)).
:- initialize_dialect.

spec_pre(Pred,SpecPre) :-
    ((Pred = _/Arity; Pred = _:_/Arity) -> true ; type_error("<optional-module>:<predname>/<arity>",Pred)),
    (length(SpecPre,Arity) -> true ; domain_error('length of spec list',Arity)),
    maplist(valid_spec,SpecPre).

spec_post(Pred,SpecPre,SpecPost) :-
    ((Pred = _/Arity; Pred = _:_/Arity) -> true ; type_error("<optional-module>:<predname>/<arity>",Pred)),
    (length(SpecPre,Arity) -> true ; domain_error('length of spec list',Arity)),
    (length(SpecPost,Arity) -> true ; domain_error('length of spec list',Arity)),
    maplist(valid_spec,SpecPre),
    maplist(valid_spec,SpecPost).

spec_invariant(Pred,SpecInv) :-
    ((Pred = _/Arity; Pred = _:_/Arity) -> true ; type_error("<optional-module>:<predname>/<arity>",Pred)),
    (length(SpecInv,Arity) -> true ; domain_error('length of spec list',Arity)),
    maplist(valid_spec,SpecInv).

valid_spec(X) :-
    spec_and_nonvar(X),!.
valid_spec(Spec) :-
    domain_error('valid_spec',Spec).

spec_and_nonvar(Spec) :-
    nonvar(Spec),
    spec(Spec).

match_specs([],[]) :- !.
match_specs([E|T],[E|S]) :-
    !, match_specs(T,S).
match_specs([_|T],[specvar(_)|S]) :-
    !, match_specs(T,S).
match_specs([specvar(_)|T],[_|S]) :-
    !, match_specs(T,S).

spec(var).
spec(ground).
spec(nonvar).
spec(any).
spec(specvar(X)) :- var(X),!.
spec(specvar(X)) :- compound(X), \+ ground(X), spec(X).

% Definition of spec predicates
spec(atomic).
spec(atom).
spec(string).
spec(atom(X)) :- atom(X).
spec(integer).
spec(number).
spec(float).
spec(emptylist).
spec(compound).

spec(compound(X)) :- compound(X), X=.. [_|Args], maplist(spec_and_nonvar,Args).
spec(list(X)) :- spec_and_nonvar(X).
spec(tuple(X)) :- is_list(X),maplist(spec_and_nonvar,X).

spec(and(L)) :- is_list(L), maplist(spec_and_nonvar,L).
spec(one_of(L)) :- is_list(L), maplist(spec_and_nonvar,L).
spec(Spec) :-
    compound(Spec),!,
    Spec =.. [Functor|Arglist],
    maplist(spec_and_nonvar,Arglist),
    spec_alias(NewSpec,_),
    NewSpec =.. [Functor|NewArglist],
    match_specs(Arglist,NewArglist),!.
spec(Spec) :-
    \+ compound(Spec),
    spec_alias(Spec,_).


:- dynamic spec_alias/2.
spec_alias(int,integer).


check_if_vars_are_wrapped(Spec) :-
    ground(Spec),!.
check_if_vars_are_wrapped(Spec) :-
    compound(Spec),
    Spec =.. [specvar,_],!.
check_if_vars_are_wrapped(Spec) :-
    compound(Spec),
    Spec =.. [_|Arglist],
    maplist(check_if_vars_are_wrapped,Arglist).

declare_spec(SpecName) :-
    \+ spec(SpecName),
    check_if_vars_are_wrapped(SpecName),
    assert(spec_alias(SpecName,empty)).

define_spec(SpecName,SpecAlias) :-
    % only allow ground or vars wrapped in any
    check_if_vars_are_wrapped(SpecName),
    valid_spec(SpecAlias),
    assert(spec_alias(SpecName,SpecAlias)),
    retract(spec_alias(SpecName,empty)), !.
