:- module(prolog_analyzer,[enable_write_out/0,spec_pre/2,spec_post/3,spec_invariant/2,define_spec/2,declare_spec/1]).
:- use_module(library(error)).

:- multifile term_expansion/2.
:- dynamic write_out/0.
:- public enable_write_out/0.

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
spec(atom(X)) :- atom(X).
spec(integer).
spec(number).
spec(float).

spec(compound(X)) :- compound(X), compound_name_arguments(X,_,Args), maplist(spec_and_nonvar,Args).
spec(list(X)) :- spec_and_nonvar(X).
spec(tuple(X)) :- is_list(X),maplist(spec_and_nonvar,X).

spec(and(L)) :- is_list(L), maplist(spec_and_nonvar,L).
spec(one_of(L)) :- is_list(L), maplist(spec_and_nonvar,L).
spec(Spec) :-
    compound(Spec),!,
    Spec =.. [Functor|Arglist],
    maplist(spec_and_nonvar,Arglist),
    spec_alias(NewSpec,_),
    NewSpec =.. [Functor|NewArglist],!,
    match_specs(Arglist,NewArglist).
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



% Transform to edn
enable_write_out :-
    prolog_load_context(file,File),
    get_clojure_file_name(File,ClojureFile),
    (access_file(ClojureFile,write), exists_file(ClojureFile) -> delete_file(ClojureFile); true),
    assertz(write_out).

multi_string_concat([H],H) :- !.

multi_string_concat([X,Y|T],Res) :-
    string_concat(X,Y,R),
    multi_string_concat([R|T],Res).

join(_,[X],X) :- !.
join(Sep,[X,Y|T],Res) :-
    string_concat(X,Sep,XKomma),
    string_concat(XKomma,Y,XKommaY),
    join(Sep,[XKommaY|T],Res).

mapcat(_Goal,[],_,"[]",_) :- !.
mapcat(Goal,List,Sep,Res,Break) :-
    maplist(Goal,List,List2),
    join(Sep,List2,Join),
    (Break == true ->
         multi_string_concat(["[\n",Join,"]"],Res)
     ;
         multi_string_concat(["[",Join,"]"],Res)
    ).

indent_elements(_,[],[]) :- !.
indent_elements(Indent,[H|T],[Indent/H|S]) :-
    indent_elements(Indent,T,S).

string_concat_direct(String,OtherString,Res) :-
    sub_string(String,X,1,0,P),
    sub_string(String,0,X,1,WithoutNewLine),
    (P == "\n" ->
         string_concat(WithoutNewLine,OtherString,Res); string_concat(String,OtherString,Res)).
concat_to_last_elem(List,String,Res) :-
    reverse(List,[Ind/Last|Other]),!,
    sub_string(Last,X,1,0,P),
    sub_string(Last,0,X,1,WithOutNewLine),
    (P == "\n" ->
         string_concat(WithOutNewLine,String,NewLast); string_concat(Last,String,NewLast)),
    reverse([Ind/NewLast|Other],Res).

concat_to_last_elem(List,String,Res) :-
    reverse(List,[Last|Other]),!,
    sub_string(Last,X,1,0,P),
    sub_string(Last,0,X,1,WithOutNewLine),
    (P == "\n" ->
         string_concat(WithOutNewLine,String,NewLast); string_concat(Last,String,NewLast)),
    reverse([NewLast|Other],Res).

create_indent(0,"") :- !.
create_indent(N,Res) :-
    M is N-1,
    create_indent(M,Acc),
    string_concat(Acc," ",Res).

rule_to_map(Head,Body,Module,Map) :-
    split(Head,Name,Arity,Arglist,_),
    create_arglist(Arglist,13,ResArglist),
    create_body(25,Body,BodyRes),
    multi_string_concat(["{:name     \"",Name,"\""],Goal_Elem),
    multi_string_concat([":module   \"",Module,"\""],Module_Elem),
    string_concat(":arity    ",Arity,Arity_Elem),
    append([0/Goal_Elem,13/Module_Elem,13/Arity_Elem],ResArglist,List1),
    append(List1,BodyRes,List2),
    concat_to_last_elem(List2,"}",List3),
    create_map(List3,Map).

goal_to_map(FirstLineInd,OtherLineInd,if(Cond,Then),Map) :-
    !,
    string_concat("{:goal     ",":if",Goal_Elem),
    string_concat(":arity    ",2,Arity_Elem),
    maplist(create_body_list(26),[Cond,Then],TMP),
    maplist(create_map,TMP,[H|Maps]),
    string_concat(":arglist  [",H,NewH),
    concat_to_last_elem(Maps,"]}",NewMaps),
    indent_elements(20,NewMaps,BLABLA),
    append([FirstLineInd/Goal_Elem,OtherLineInd/Arity_Elem,OtherLineInd/NewH],BLABLA,List),
    create_map(List,Map).

goal_to_map(FirstLineInd,OtherLineInd,or(Arglist),Map) :- !,
    length(Arglist,Arity),
    string_concat("{:goal     ",":or",Goal_Elem),
    string_concat(":arity    ",Arity,Arity_Elem),
    maplist(create_body_list(26),Arglist,TMP),
    maplist(create_map,TMP,[H|Maps]),
    string_concat(":arglist  [",H,NewH),
    concat_to_last_elem(Maps,"]}",NewMaps),
    indent_elements(20,NewMaps,BLABLA),
    append([FirstLineInd/Goal_Elem,OtherLineInd/Arity_Elem,OtherLineInd/NewH],BLABLA,List),
    create_map(List,Map).

goal_to_map(FirstLineInd,OtherLineInd,Goal,Map) :-
    split(Goal,Name,Arity,Arglist,Module),
    multi_string_concat(["{:goal     \"",Name,"\""],Goal_Elem),
    multi_string_concat([":module   \"",Module,"\""],Module_Elem),
    string_concat(":arity    ",Arity,Arity_Elem),
    create_arglist(Arglist,OtherLineInd,ResArglist),
    append([FirstLineInd/Goal_Elem,OtherLineInd/Module_Elem,OtherLineInd/Arity_Elem],ResArglist,List),
    concat_to_last_elem(List,"}",List2),
    create_map(List2,Map).

create_body(Ind,Body,[13/NewH|T]) :-
    create_body_list(Ind,Body,[0/H|T]),
    string_concat(":body     ",H,NewH).


create_body_list(_,[],[0/"[]"]) :- !.
create_body_list(Ind,[B],[0/Res]) :-
    !,
    goal_to_map(0,Ind,B,H),
    string_concat("[",H,Tmp1),
    string_concat_direct(Tmp1,"]",Res).
create_body_list(Ind,[B|Body],Res) :-
    goal_to_map(0,Ind,B,H),
    X is Ind-1,
    maplist(goal_to_map(X,Ind),Body,T),
    string_concat("[",H,Line1),
    concat_to_last_elem(T,"]",Tmp1),
    indent_elements(0,Tmp1,Tmp2),
    append([0/Line1],Tmp2,Res).


create_arglist([],Indent,[Indent/":arglist  []"]) :- !.
create_arglist(Arglist,Indent,[Indent/Res]) :-
    maplist(arg_to_map,Arglist,[T]),!,
    string_concat(":arglist  [",T,Tmp1),
    string_concat_direct(Tmp1,"]",Res).
create_arglist(Arglist,Indent,Res) :-
    maplist(arg_to_map,Arglist,[H|T]),
    string_concat(":arglist  [",H,Line1),
    concat_to_last_elem(T,"]",Tmp1),
    X is Indent+11,
    indent_elements(X,Tmp1,Tmp2),
    append([Indent/Line1],Tmp2,Res).

create_map(List,Res) :-
    create_map(List,"",Res).
create_map([],Res,Res) :- !.
create_map([Indent/H|T],Acc,Res) :-
    create_indent(Indent,IndentString),
    multi_string_concat([Acc,IndentString,H,"\n"],NewAcc),
    create_map(T,NewAcc,Res).

arg_to_map(Arg,Map) :-
    string(Arg),!,
    arg_to_map(string,Arg,Map).
arg_to_map(Arg,Map) :-
    atom(Arg),!,
    arg_to_map(atom,Arg,Map).
arg_to_map(Arg,Map) :-
    integer(Arg),!,
    arg_to_map(integer,Arg,Map).
arg_to_map(Arg,Map) :-
    float(Arg),!,
    arg_to_map(float,Arg,Map).
arg_to_map(Arg,Map) :-
    number(Arg),!,
    arg_to_map(number,Arg,Map).
arg_to_map(Arg,Map) :-
    atomic(Arg),!,
    arg_to_map(atomic,Arg,Map).
arg_to_map(Arg,Map) :-
    compound(Arg),!,
    arg_to_map(compound,Arg,Map).
arg_to_map(Arg,Map) :-
    ground(Arg),!,
    arg_to_map(ground,Arg,Map).
arg_to_map(Arg,Map) :-
    var(Arg),!,
    arg_to_map(var,Arg,Map).

arg_to_map(Arg,Map) :-
    arg_to_map(any,Arg,Map).


arg_to_map(compound,Term,Map) :-
    Term =.. ['[|]'|[Head,Tail]],
    arg_to_map(Head,HeadString),
    arg_to_map(Tail,TailString),

    multi_string_concat(["{:type :list"],TypePart),
    multi_string_concat([":head ",HeadString],HeadPart),
    multi_string_concat([":tail ",TailString,"}"],TailPart),

    create_map([0/TypePart,1/HeadPart,1/TailPart],Map).

arg_to_map(compound,Term,Map) :-
    !,
    Term =.. [Functor|Args],
    term_string(Functor,FunctorString),
    create_arglist(Args,0,Arglist),
    multi_string_concat(["{:type :compound"],TypePart),
    multi_string_concat([":functor \"",FunctorString,"\""],FunctorPart),

    append([0/TypePart,1/FunctorPart],Arglist,List1),
    append(List1,[0/"}"],List2),
    create_map(List2,Map).

arg_to_map(var,Term,Map) :-
    !,
    (var_property(Term,name(Name)) -> true ; term_string(Term,Name)),
    (atom_codes(Name,[95|_]) -> Type = "anon_var" ; Type = "var"),
    multi_string_concat(["{:name \"", Name, "\" :type :", Type, "}"],Map).


arg_to_map(string,_,"{:type :string}") :- !.

arg_to_map(Type,Term,Map) :-
    (Type = integer; Type = number; Type = float),
    !,
    term_string(Term,String),
    string_concat("{:value ", String,R1),
    string_concat(R1, " :type :", R2),
    string_concat(R2, Type, R3),
    string_concat(R3, "}",Map).

arg_to_map(Type,Term,Map) :-
    term_string(Term,String),
    string_concat("{:term \"", String,R1),
    string_concat(R1, "\" :type :", R2),
    string_concat(R2, Type, R3),
    string_concat(R3, "}",Map).


split(Module:Term,Name,Arity,Arglist,Module) :-
    !,
    split(Term,Name,Arity,Arglist,_).

split(Term,Name,Arity,Arglist,self) :-
    functor(Term,Name1,Arity),
    (Name1 = (\+) -> Name = ":not" ; Name = Name1),
    Term =.. [_|Arglist].

expand(Term,Module) :-
    current_output(Out),
    expand(Term,Module,Out).

expand(_,term_expander,_) :- !.

expand(':-'(A,B),Module,Stream) :-
    !,
    body_list(B,Body),
    Start = "{:type      :pred\n :content   ",
    rule_to_map(A,Body,Module,Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).

%special cases
expand(':-'(spec_pre(InternalModule:Functor/Arity,Arglist)),_Module,Stream) :-
    !,
    Start = "{:type :spec_pre\n:content ",
    goal_to_map(0,13,spec_pre(InternalModule:Functor/Arity,Arglist),Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).

expand(':-'(spec_pre(Functor/Arity,Arglist)),Module,Stream) :-
    !,
    expand(':-'(spec_pre(Module:Functor/Arity,Arglist)),Module,Stream).

expand(':-'(spec_post(InternalModule:Functor/Arity,Arglist1,Arglist2)),_Module,Stream) :-
    !,
    Start = "{:type :spec_post\n:content ",
    goal_to_map(0,13,spec_post(InternalModule:Functor/Arity,Arglist1,Arglist2),Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).

expand(':-'(spec_post(Functor/Arity,Arglist1,Arglist2)),Module,Stream) :-
    !,
    expand(':-'(spec_post(Module:Functor/Arity,Arglist1,Arglist2)),Module,Stream).

expand(':-'(spec_invariant(InternalModule:Functor/Arity,Arglist)),_Module,Stream) :-
    !,
    Start = "{:type :spec_inv\n:content ",
    goal_to_map(0,13,spec_invariant(InternalModule:Functor/Arity,Arglist),Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).


expand(':-'(spec_invariant(Functor/Arity,Arglist)),Module,Stream) :-
    !,
    expand(':-'(spec_invariant(Module:Functor/Arity,Arglist)),Module,Stream).

expand(':-'(A),_Module,Stream) :-
    A = declare_spec(_),
    !,
    Start = "{:type :declare_spec\n:content ",
    goal_to_map(0,13,A,Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).
expand(':-'(A),_Module,Stream) :-
    A  = define_spec(_,_),
    !,
    Start = "{:type :define_spec\n:content ",
    goal_to_map(0,13,A,Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).
expand(':-'(enable_write_out),_,_) :- !.

% normal direct call
expand(':-'(A),_Module,Stream) :-
    !,
    Start = "{:type      :direct\n :content   ",
    goal_to_map(0,13,A,Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).

% fact
expand((C),Module,Stream) :-
    !,
    expand(':-'(C,true),Module,Stream).

body_list(Body,List) :-
    transform(Body,E),
    (is_list(E) -> List = E; List = [E]).

transform(Body,or(SimpleOr)) :-
    Body =.. [(;),Left,Right],!,
    transform(Left,LeftList),
    transform(Right,RightList),
    Res = [LeftList,RightList],
    simplify_or(or(Res),or(SimpleOr)).

transform(Body,Res) :-
    Body =.. [(,),Left,Right],!,
    transform(Left,LeftList),
    transform(Right,RightList),
    merge_list(LeftList,RightList,Res).

transform(Body,[if(LeftList,RightList)]) :-
    Body =.. [(->),Left,Right],!,
    transform(Left,LeftList),
    transform(Right,RightList).

transform(Body,[Body]).


simplify_or(or([]),or([])) :- !.
simplify_or(or([or(L)]),or(L)) :- !.
simplify_or(or([or(X)|T]),Res) :- !,
    append(X,T,New),
    simplify_or(or(New),Res).
simplify_or(or([H|T]),or([H|S])) :-
    simplify_or(or(T),or(S)).


merge_list(L,R,New) :-
    is_list(L), is_list(R),!,
    append(L,R,New).
merge_list(L,R,[L|R]) :-
    \+is_list(L), is_list(R),!.
merge_list(L,R,Res) :-
    is_list(L),  \+is_list(R),!,
    append(L,[R],Res).
merge_list(L,R,[L,R]).

user:term_expansion(A,A) :-
    !,
    prolog_load_context(module,Module),
    (write_out, main_file(File) ->
         get_clojure_file_name(File,ClojureFile),
         open(ClojureFile,append,Stream),
         expand(A,Module,Stream),
         write(Stream,";; ----------------"),nl(Stream),nl(Stream),
         close(Stream)
    ).


get_clojure_file_name(File,ClojureFile) :-
    string_concat(File,".edn",ClojureFile).

main_file(File) :-
    prolog_load_context(file,File),
    prolog_load_context(source,File),
    source_file_property(File,module(_)).
