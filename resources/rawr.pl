:- module(rawr,[]).

raw_machine_term(machine(M),'',M).
raw_machine_term(generated(_,M),'',M).
raw_machine_term(machine_header(_,Name,_Params),Name,[]). % TO DO: treat Params
raw_machine_term(abstract_machine(_,_,Header,M),'MACHINE',[Header,M]).
raw_machine_term(properties(_,P),'PROPERTIES',P).
raw_machine_term(operations(_,P),'OPERATIONS',P).
raw_machine_term(definitions(_,P),'DEFINITIONS',P).
raw_machine_term(constants(_,P),'CONSTANTS',P).
raw_machine_term(variables(_,P),'VARIABLES',P).
raw_machine_term(invariant(_,P),'INVARIANT',P).
raw_machine_term(assertions(_,P),'ASSERTIONS',P).
raw_machine_term(constraints(_,P),'CONSTRAINTS',P).
raw_machine_term(sets(_,P),'SETS',P).
raw_machine_term(deferred_set(_,P),P,[]). % TO DO: enumerated_set ...
%raw_machine_term(identifier(_,P),P,[]).
