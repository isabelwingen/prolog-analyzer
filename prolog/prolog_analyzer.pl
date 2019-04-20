:- module(prolog_analyzer,[set_file/1]).
:- use_module(library(lists)).

initialize_dialect :-
    prolog_load_context(dialect,swi),!,
    use_module(library(error)),
    use_module(library(apply)),
    use_module(library(lists)).

initialize_dialect :-
    use_module(library(file_systems)),
    use_module(library(codesio)),
    use_module(library(lists)).

:- initialize_dialect.
:- multifile term_expansion/2.
:- dynamic write_out/0.
:- dynamic filename/1.
:- public enable_write_out/0.
:- public set_file/1.

set_file(Filename) :-
    retractall(filename(_)),
    assert(filename(Filename)).

sicstus_transform(Term,Res) :-
    number(Term),!,
    number_codes(Term,Res).
sicstus_transform(Term,Res) :-
    atom(Term),!,
    atom_codes(Term,Res).
sicstus_transform(Term,Term).

my_string_concat(A,B,C) :-
    prolog_load_context(dialect,swi),!,
    string_concat(A,B,C).
my_string_concat(A,B,C) :-
    sicstus_transform(A,AA),
    sicstus_transform(B,BB),
    append(AA,BB,X),
    (atom(X) -> C=X;atom_codes(C,X)).

multi_string_concat([H],H) :- !.

multi_string_concat([X,Y|T],Res) :-
    my_string_concat(X,Y,R),
    multi_string_concat([R|T],Res).

join(_,[X],X) :- !.
join(Sep,[X,Y|T],Res) :-
    my_string_concat(X,Sep,XKomma),
    my_string_concat(XKomma,Y,XKommaY),
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

concat_to_last_elem(List,String,Res) :-
    reverse(List,[Last|Other]),!,
    my_string_concat(Last,String,NewLast),
    reverse([NewLast|Other],Res).

rule_to_map(Head,Body,Module,Map) :-
    split(Head,Name,Arity,Arglist,_),
    create_arglist(Arglist,ResArglist),
    create_body(Body,BodyRes),
    multi_string_concat(["{:name     \"",Name,"\""],Goal_Elem),
    multi_string_concat([":module   \"",Module,"\""],Module_Elem),
    my_string_concat(":arglist ",ResArglist,Arglist_Elem),
    my_string_concat(":arity    ",Arity,Arity_Elem),
    List2 = [Goal_Elem,Module_Elem,Arity_Elem,Arglist_Elem,BodyRes],
    create_map(List2,Map2),
    my_string_concat(Map2,"}\n",Map).

goal_to_map(if(Cond,Then),Map) :-
    !,
    create_body_list(Cond,CondBody),
    create_body_list(Then,ThenBody),
    create_map([CondBody,ThenBody],Body),
    my_string_concat("{:goal ",":if",Goal_Elem),
    my_string_concat(":arity ","2",Arity_Elem),
    multi_string_concat([":arglist [",Body,"]"],Arglist_Elem),
    List = [Goal_Elem,Arity_Elem,Arglist_Elem,"}\n"],
    create_map(List,Map).

goal_to_map(or(Arglist),Map) :-
    !,
    length(Arglist,Arity),
    maplist(create_body_list,Arglist,Tmp),
    create_map(Tmp,Body),
    my_string_concat("{:goal     ",":or",Goal_Elem),
    my_string_concat(":arity    ",Arity,Arity_Elem),
    multi_string_concat([":arglist [",Body,"]"],Arglist_Elem),
    List = [Goal_Elem,Arity_Elem,Arglist_Elem,"}\n"],
    create_map(List,Map).


goal_to_map(Goal,Map) :-
    split(Goal,Name,Arity,Arglist,Module),
    multi_string_concat(["{:goal     \"",Name,"\""],Goal_Elem),
    multi_string_concat([":module   \"",Module,"\""],Module_Elem),
    my_string_concat(":arity    ",Arity,Arity_Elem),
    create_arglist(Arglist,ResArglist),
    my_string_concat(":arglist ",ResArglist,Arglist_Elem),
    create_map([Goal_Elem,Module_Elem,Arity_Elem, Arglist_Elem],Map1),
    my_string_concat(Map1,"}\n",Map).

create_body(Body,BodyString) :-
    create_body_list(Body,BodyList),
    my_string_concat(":body     ",BodyList,BodyString).


create_body_list([],"[]") :- !.
create_body_list([B],Res) :-
    !,
    goal_to_map(B,H),
    multi_string_concat(["[",H,"]"],Res).
create_body_list(Body,Res) :-
    maplist(goal_to_map,Body,T),
    join(", ",T,String),
    multi_string_concat(["[",String,"]"],Res).


create_arglist([],"[]") :- !.
create_arglist([A],Res) :- !,
    arg_to_map(A,B),
    multi_string_concat(["[",B,"]"],Res).
create_arglist(Arglist,Res) :-
    maplist(arg_to_map,Arglist,Maps),
    join(", ",Maps,String),
    multi_string_concat(["[",String,"]"],Res).

create_map(List,Res) :-
    create_map(List,'',Res).
create_map([],Res,Res) :- !.
create_map([H|T],Acc,Res) :-
    multi_string_concat([Acc,H,'\n'],NewAcc),
    create_map(T,NewAcc,Res).

arg_to_map(Arg,Map) :-
    prolog_load_context(dialect,swi),
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
    var(Arg),!,
    arg_to_map(var,Arg,Map).

arg_to_map(Arg,Map) :-
    arg_to_map(error,Arg,Map).


arg_to_map(compound,Term,Map) :-
    (Term =.. ['[|]'|[Head,Tail]]; Term =.. ['.'|[Head,Tail]]),!,
    arg_to_map(Head,HeadString),
    arg_to_map(Tail,TailString),

    multi_string_concat(["{:type :list"],TypePart),
    multi_string_concat([":head ",HeadString],HeadPart),
    multi_string_concat([":tail ",TailString,"}"],TailPart),

    create_map([TypePart,HeadPart,TailPart],Map).

arg_to_map(compound,Term,Map) :-
    !,
    Term =.. [FunctorString|Args],
    create_arglist(Args,Arglist),
    multi_string_concat(["{:type :compound"],TypePart),
    multi_string_concat([":functor \"",FunctorString,"\""],FunctorPart),
    my_string_concat(":arglist ",Arglist,Arglist_Elem),
    create_map([TypePart,FunctorPart, Arglist_Elem],Map1),
    my_string_concat(Map1,"}\n",Map).

arg_to_map(var,Term,Map) :-
    prolog_load_context(dialect,swi),!,
    (var_property(Term,name(Name)) -> true ; term_string(Term,Name)),
    (atom_codes(Name,[95|_]) -> Type = "anon_var" ; Type = "var"),
    multi_string_concat(["{:name \"", Name, "\" :type :", Type, "}"],Map).

arg_to_map(var,Term,Map) :-
    !,
    write_to_codes(Term,NameCodes),
    atom_codes(Name,NameCodes),
    multi_string_concat(["{:name \"", Name, "\" :type :var}"], Map).


arg_to_map(string,Term,M) :-
    !,
    term_string(Term,S),
    my_string_concat("{:type :string :term ", S, R1),
    my_string_concat(R1,"}",M).

arg_to_map(Type,Term,Map) :-
    (Type = integer; Type = number; Type = float),
    prolog_load_context(dialect,swi),
    !,
    term_string(Term,String),
    my_string_concat("{:value ", String,R1),
    my_string_concat(R1, " :type :", R2),
    my_string_concat(R2, Type, R3),
    my_string_concat(R3, "}",Map).

arg_to_map(Type,Term,Map) :-
    (Type = integer; Type = number; Type = float),
    !,
    number_codes(Term,NumberAsCodes),
    my_string_concat("{:value ", NumberAsCodes,R1),
    my_string_concat(R1, " :type :", R2),
    my_string_concat(R2, Type, R3),
    my_string_concat(R3, "}",Map).


arg_to_map(atom,Term,Map) :-
    prolog_load_context(dialect,swi),!,
    term_string(Term,String),
    my_string_concat("{:term \"", String,R1),
    my_string_concat(R1, "\" :type :atom}", Map).

arg_to_map(atom,Term,Map) :-
    !,
    my_string_concat("{:term \"", Term,R1),
    my_string_concat(R1, "\" :type :atom}", Map).

arg_to_map(atomic,[],"{:type :empty-list}") :- !.
arg_to_map(error,Term,Map) :-
    (prolog_load_context(dialect,swi) -> term_string(Term, String); Term = String),
    multi_string_concat(["{:type :should-not-happen :term ",String, "}"], Map).


split(Module:Term,unknown,-1,[],unknown) :-
    var(Module),
    var(Term),!.

split(Module:Term,Name,Arity,Arglist,Module) :-
    !,
    functor(Term,Name1,Arity),
    (Name1 = (\+) -> Name = ":not" ; Name = Name1),
    Term =.. [_|Arglist].

split(Term,Name,Arity,Arglist,self) :-
    functor(Term,Name1,Arity),
    (Name1 = (\+) -> Name = ":not" ; Name = Name1),
    Term =.. [_|Arglist].

expand(Term,Module) :-
    current_output(Out),
    expand(Term,Module,Out).

expand(_,term_expander,"") :- !.

expand(_,annotations,"") :- !.

expand(':-'(A,B),Module,Result) :-
    !,
    body_list(B,Body),
    Start = "{:type      :pred\n :content   ",
    rule_to_map(A,Body,Module,Map),
    my_string_concat(Start,Map,Tmp1),
    my_string_concat(Tmp1,"}",Result).

%special cases
expand(':-'(spec_pre(InternalModule:Functor/Arity,Arglist)),_Module,Result) :-
    !,
    Start = "{:type :spec_pre\n:content ",
    goal_to_map(spec_pre(InternalModule:Functor/Arity,Arglist),Map),
    my_string_concat(Start,Map,Tmp1),
    my_string_concat(Tmp1,"}\n",Result).

expand(':-'(spec_pre(Functor/Arity,Arglist)),Module,Result) :-
    !,
    expand(':-'(spec_pre(Module:Functor/Arity,Arglist)),Module,Result).

expand(':-'(spec_post(InternalModule:Functor/Arity,Arglist1,Arglist2)),_Module,Result) :-
    !,
    Start = "{:type :spec_post\n:content ",
    goal_to_map(spec_post(InternalModule:Functor/Arity,Arglist1,Arglist2),Map),
    my_string_concat(Start,Map,Tmp1),
    my_string_concat(Tmp1,"}\n",Result).

expand(':-'(spec_post(Functor/Arity,Arglist1,Arglist2)),Module,Result) :-
    !,
    expand(':-'(spec_post(Module:Functor/Arity,Arglist1,Arglist2)),Module,Result).

expand(':-'(spec_invariant(InternalModule:Functor/Arity,Arglist)),_Module,Result) :-
    !,
    Start = "{:type :spec_inv\n:content ",
    goal_to_map(spec_invariant(InternalModule:Functor/Arity,Arglist),Map),
    my_string_concat(Start,Map,Tmp1),
    my_string_concat(Tmp1,"}\n",Result).

expand(':-'(spec_invariant(Functor/Arity,Arglist)),Module,Result) :-
    !,
    expand(':-'(spec_invariant(Module:Functor/Arity,Arglist)),Module,Result).

expand(':-'(A),_Module,Result) :-
    A = declare_spec(_),
    !,
    Start = "{:type :declare_spec\n:content ",
    goal_to_map(A,Map),
    my_string_concat(Start,Map,Tmp1),
    my_string_concat(Tmp1,"}\n",Result).

expand(':-'(A),_Module,Result) :-
    A  = define_spec(_,_),
    !,
    Start = "{:type :define_spec\n:content ",
    goal_to_map(A,Map),
    my_string_concat(Start,Map,Tmp1),
    my_string_concat(Tmp1,"}\n",Result).

% normal direct call
expand(':-'(A),_Module,Result) :-
    !,
    Start = "{:type      :direct\n :content   ",
    goal_to_map(A,Map),
    my_string_concat(Start,Map,Tmp1),
    my_string_concat(Tmp1,"}\n",Result).

% fact
expand((C),Module,Result) :-
    !,
    expand(':-'(C,true),Module,Result).

body_list(Body,List) :-
    transform(Body,E),
    (is_list(E) -> List = E; List = [E]).

transform(Body,or(SimpleOr)) :-
    nonvar(Body),
    Body =.. [';',Left,Right],!,
    transform(Left,LeftList),
    transform(Right,RightList),
    Res = [LeftList,RightList],
    simplify_or(or(Res),or(SimpleOr)).

transform(Body,Res) :-
    nonvar(Body),
    Body =.. [',',Left,Right],!,
    transform(Left,LeftList),
    transform(Right,RightList),
    merge_list(LeftList,RightList,Res).

transform(Body,[if(LeftList,RightList)]) :-
    nonvar(Body),
    Body =.. ['->',Left,Right],!,
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
    filename(ClojureFile),
    expand(A,Module,Result),
    open(ClojureFile,append,Stream),
    write(Stream,Result),nl(Stream),
    write(Stream,";; ----------------"),nl(Stream),nl(Stream),
    close(Stream).

:- dynamic counter/1.
inc_counter :-
    retract(counter(D)),!,
    C is D+1,
    assert(counter(C)).
inc_counter :-
    assert(counter(0)).
dec_counter :-
    retract(counter(D)),
    C is D-1,
    assert(counter(C)).
get_counter(C) :-
    counter(C),!.
get_counter(0) :-
    assert(counter(0)).

:- multifile user:term_expansion/6.
user:term_expansion(Term, Layout1, Ids, Term, Layout1, [plspec_token|Ids]) :-
    nonmember(plspec_token, Ids),
    Term = ':-'(use_module(_)),!.
user:term_expansion(Term, Layout1, Ids, Term, Layout1, [plspec_token|Ids]) :-
    %get_counter(C),
    %print(C), print(' Term expansion: '), print(Term),nl,flush_output,
    nonmember(plspec_token, Ids),
    prolog_load_context(module, Module),
    filename(ClojureFile),
    expand(Term,Module,Result),
    open(ClojureFile,append,Stream),
    %inc_counter,
    write(Stream,Result),nl(Stream),
    %write(Stream,';; --------------------'), nl(Stream),nl(Stream),
    close(Stream).
    %dec_counter.

% must_fail_clpfd_det takes super long

% startet at 15:44
