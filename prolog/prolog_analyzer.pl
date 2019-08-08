:- module(prolog_analyzer,[set_file/1]).
:- use_module(library(lists)).

initialize_dialect :-
    prolog_load_context(dialect,swi),!,
    use_module(library(error)),
    use_module(library(system)),
    use_module(library(apply)),
    use_module(library(lists)).

initialize_dialect :-
    use_module(library(file_systems)),
    use_module(library(system)),
    use_module(library(codesio)),
    use_module(library(lists)).

:- initialize_dialect.
:- multifile term_expansion/2.
:- dynamic write_out/0.
:- dynamic filename/1.
:- public set_file/1.

set_file(Filename) :-
    retractall(filename(_)),
    assert(filename(Filename)).

ednify_codes([], []).
ednify_codes([0'\\|T], [0'\\, 0'\\|R]) :-
    !,
    ednify_codes(T, R).
ednify_codes([0'"|T], [0'\\, 0'"|R]) :-
    !,
    ednify_codes(T, R).
ednify_codes([X|T], [X|R]) :-
    ednify_codes(T, R).

ednify_atom(Atom, EdnAtom) :-
    atom_codes(Atom, Codes),
    ednify_codes(Codes, NewCodes),
    atom_codes(EdnAtom, NewCodes).

ednify_string(String, EdnString) :-
    string_codes(String, Codes),
    ednify_codes(Codes, NewCodes),
    string_codes(EdnString, NewCodes).

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

rule_to_map(Head,Body,Module,Map) :-
    split(Head,Name,Arity,Arglist,_),
    ednify_atom(Name,EdnName),
    create_arglist(Arglist,ResArglist),
    create_body(Body,BodyRes),
    multi_string_concat(["{:name     \"",EdnName,"\""],Goal_Elem),
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
    ednify_atom(Name,EdnName),
    multi_string_concat(["{:goal     \"",EdnName,"\""],Goal_Elem),
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
create_body_list(or(L),Res) :-
    !,
    create_body_list([or(L)],Res).
create_body_list(if(L),Res) :-
    !,
    create_body_list([or(L)],Res).
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
    Term =.. [Functor|Args],
    create_arglist(Args,Arglist),
    ednify_atom(Functor,FunctorString),
    multi_string_concat(["{:type :compound"],TypePart),
    multi_string_concat([":functor \"",FunctorString,"\""],FunctorPart),
    my_string_concat(":arglist ",Arglist,Arglist_Elem),
    create_map([TypePart,FunctorPart, Arglist_Elem],Map1),
    my_string_concat(Map1,"}\n",Map).

arg_to_map(var,Term,Map) :-
    prolog_load_context(dialect,swi),!,
    (var_property(Term,name(Name)) -> true ; term_string(Term,Name)),
    multi_string_concat(["{:name \"", Name, "\" :type :var}"],Map).

arg_to_map(var,Term,Map) :-
    !,
    write_to_codes(Term,NameCodes),
    atom_codes(Name,NameCodes),
    multi_string_concat(["{:name \"", Name, "\" :type :var}"], Map).


arg_to_map(string,Term,M) :-
    !,
    ednify_string(Term,EdnString),
    multi_string_concat(["{:type :string :term \"",EdnString,"\"}"],M).

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

arg_to_map(atom,[],"{:type :empty-list}") :- !.
arg_to_map(atom,Term,Map) :-
    prolog_load_context(dialect,swi),
    !,
    ednify_atom(Term,EdnAtom),
    my_string_concat("{:term \"", EdnAtom,R1),
    my_string_concat(R1, "\" :type :atom}", Map).
arg_to_map(atom,Term,Map) :-
    !,
    ednify_atom(Term,EdnTerm),
    my_string_concat("{:term \"", EdnTerm,R1),
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

spec_to_string(Term,Name) :-
    var(Term),
    prolog_load_context(dialect,swi),!,
    (var_property(Term,name(Name)) -> true ; term_string(Term,Name)).
spec_to_string(Term,Name) :-
    var(Term),!,
    write_to_codes(Term,NameCodes),
    atom_codes(Name,NameCodes).
spec_to_string(Terms,String) :-
    is_list(Terms),!,
    maplist(spec_to_string,Terms,Strings),
    join(", ",Strings,Inner),
    multi_string_concat(["[",Inner,"]"],String).
spec_to_string(Term,String) :-
    atomic(Term),!,
    ednify_atom(Term,EdnTerm),
    multi_string_concat(["{:type :",EdnTerm,"}"],String).
spec_to_string(atom(Atom),String) :-
    !,
    ednify_atom(Atom,EdnAtom),
    multi_string_concat(["{:type :same :term \"",EdnAtom,"\"}"],String).
spec_to_string(same(Atom),String) :-
    !,
    ednify_atom(Atom,EdnAtom),
    multi_string_concat(["{:type :same :term \"",EdnAtom,"\"}"],String).
spec_to_string(one_of(Arglist),String) :-
    !,
    spec_to_string(Arglist,Inner),
    multi_string_concat(["{:type :one-of :arglist ",Inner,"}"],String).
spec_to_string(and(Arglist),String) :-
    !,
    spec_to_string(Arglist,Inner),
    multi_string_concat(["{:type :and :arglist ",Inner,"}"],String).
spec_to_string(tuple(Arglist),String) :-
    !,
    spec_to_string(Arglist,Inner),
    multi_string_concat(["{:type :tuple :arglist  ",Inner,"}"],String).
spec_to_string(list(Type),String) :-
    !,
    spec_to_string(Type,Inner),
    multi_string_concat(["{:type :list :list-type ",Inner,"}"],String).
spec_to_string(compound(Compound),String) :-
    !,
    Compound =.. [Functor|Arglist],
    ednify_atom(Functor,FunctorString),
    spec_to_string(Arglist,Inner),
    multi_string_concat(["{:type :compound :functor \"",FunctorString,"\" :arglist ",Inner,"}"],String).

spec_to_string(specvar(X),String) :-
    !,
    spec_to_string(X,Inner),
    multi_string_concat(["{:type :specvar :name \"",Inner,"\"}"],String).
spec_to_string(placeholder(X),String) :-
    !,
    ednify_atom(X,Inner),
    multi_string_concat(["{:type :placeholder :name \"",Inner,"\"}"],String).


spec_to_string(Userdefspec,String) :-
    compound(Userdefspec),
    Userdefspec =.. [Name|Arglist],
    ednify_atom(Name,EdnName),
    spec_to_string(Arglist,Inner),
    multi_string_concat(["{:type :userdef :name \"",EdnName,"\" :arglist ",Inner,"}"],String).

guard_format([],"[]") :- !.
guard_format(L, Res) :-
    is_list(L), !,
    maplist(guard_format, L, S),
    join(", ", S, P),
    multi_string_concat(["[",P,"]"], Res).
guard_format(Id:Type, Res) :-
    !,
    spec_to_string(Type,S),
    multi_string_concat(["{:id ", Id, " :type ", S, "}"], Res).

conclusion_or_list([A;B],[A|T]) :-
    !,
    conclusion_or_list([B],T).
conclusion_or_list(X,X).

conclusion_format(Conc, Res) :-
    conclusion_or_list(Conc, Or),
    guard_format(Or,Res).

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
    Start = "{:type :pre-spec :content {:goal :spec-pre",
    ednify_atom(Functor,EdnFunctor),
    multi_string_concat([":module \"",InternalModule,"\""],ModulePart),
    multi_string_concat([":functor \"",EdnFunctor,"\""],FunctorPart),
    my_string_concat(":arity ",Arity,ArityPart),
    spec_to_string(Arglist,Spec),
    my_string_concat(":arglist ",Spec,ArglistPart),
    End = "}}",
    create_map([Start,ModulePart,FunctorPart,ArityPart,ArglistPart,End],Result).
expand(':-'(spec_pre(Functor/Arity,Arglist)),Module,Result) :-
    !,
    expand(':-'(spec_pre(Module:Functor/Arity,Arglist)),Module,Result).

expand(':-'(spec_post(InternalModule:Functor/Arity,Arglist1,Arglist2)),_Module,Result) :-
    !,
    Start = "{:type :post-spec :content {:goal :spec-post",
    ednify_atom(Functor,EdnFunctor),
    multi_string_concat([":module \"",InternalModule,"\""],ModulePart),
    multi_string_concat([":functor \"",EdnFunctor,"\""],FunctorPart),
    my_string_concat(":arity ",Arity,ArityPart),
    guard_format(Arglist1,Premisse),
    conclusion_format(Arglist2,Conclusion),
    my_string_concat(":guard ",Premisse,PremissePart),
    my_string_concat(":conclusion ",Conclusion,ConclusionPart),
    End = "}}",
    create_map([Start,ModulePart,FunctorPart,ArityPart,PremissePart,ConclusionPart,End],Result).


expand(':-'(spec_post(Functor/Arity,Arglist1,Arglist2)),Module,Result) :-
    !,
    expand(':-'(spec_post(Module:Functor/Arity,Arglist1,Arglist2)),Module,Result).

expand(':-'(spec_invariant(InternalModule:Functor/Arity,Arglist)),_Module,Result) :-
    !,
    Start = "{:type :inv-spec :content {:goal :spec-inv",
    ednify_atom(Functor,EdnFunctor),
    multi_string_concat([":module \"",InternalModule,"\""],ModulePart),
    multi_string_concat([":functor \"",EdnFunctor,"\""],FunctorPart),
    my_string_concat(":arity ",Arity,ArityPart),
    spec_to_string(Arglist,Spec),
    my_string_concat(":arglist ",Spec,ArglistPart),
    End = "}}",
    create_map([Start,ModulePart,FunctorPart,ArityPart,ArglistPart,End],Result).

expand(':-'(spec_invariant(Functor/Arity,Arglist)),Module,Result) :-
    !,
    expand(':-'(spec_invariant(Module:Functor/Arity,Arglist)),Module,Result).

expand(':-'(declare_spec(Spec)),_Module,Result) :-
    !,
    spec_to_string(Spec,Inner),
    multi_string_concat(["{:type :declare-spec :content ",Inner,"}"], Result).

expand(':-'(define_spec(Alias,Definition)),_Module,Result) :-
    !,
    spec_to_string(Alias,AliasPart),
    spec_to_string(Definition,DefPart),
    create_map(["{:type :define-spec :content {:alias ",AliasPart," :definition ", DefPart, "}}"],Result).

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


:- dynamic edn_stream/2.

get_stream(_,Stream) :-
    edn_stream(_,Stream),!.
get_stream(Module,Stream) :-
    prolog_load_context(dialect,swi),!,
    filename(ClojureFile),
    open(ClojureFile,append,Stream, [encoding('utf8')]),
    assert(edn_stream(Module,Stream)).
get_stream(Module,Stream) :-
    filename(ClojureFile),
    open(ClojureFile,append,Stream, [encoding('UTF-8')]),
    assert(edn_stream(Module,Stream)).


transform_pred_list(Pred/Arity,R) :-
    !,
    multi_string_concat(["[\"",Pred,"\", ",Arity,"]"],R).

get_pred_string(all,":all").
get_pred_string(Preds,Res) :-
    maplist(transform_pred_list,Preds,PredsAsString),
    join(", ",PredsAsString,PQ),
    multi_string_concat(["[",PQ,"]"],Res).


term_expander(end_of_file) :- !,
    prolog_load_context(module,Module),
    (retract(edn_stream(Module,Stream)) -> close(Stream); true).
term_expander(':-'(module(Module))) :-
    term_expander(':-'(module(Module,_))),!.
term_expander(':-'(module(Module,_))) :-
    !,
    prolog_load_context(file,File),
    prolog_load_context(directory,ModulePath),
    multi_string_concat(["{:type :module :content {:path \"",File,"\" :module \"",Module,"\"",":partial false}}"],Result1),
    multi_string_concat(["{:type :module :content {:path \"",ModulePath,"\" :module \"",Module,"\"",":partial true}}"],Result2),
    get_stream(Module,Stream),
    write(Stream,Result1),nl(Stream),
    write(Stream,Result2),nl(Stream),
    flush_output(Stream),!.

term_expander(':-'(use_module(library(Lib),Preds))) :-
    !,
    prolog_load_context(module, Module),
    get_pred_string(Preds,PredString),
    multi_string_concat(["{:type :use-module :content {:lib \"",Lib,"\" :in \"",Module,"\" :preds ",PredString,"}}"],Result),
    get_stream(Module,Stream),
    write(Stream,Result),nl(Stream),flush_output(Stream),!.
term_expander(':-'(use_module(library(Lib)))) :-
    term_expander(':-'(use_module(library(Lib),all))),!.

term_expander(':-'(use_module(UsedModule))) :-
    term_expander(':-'(use_module(UsedModule,all))),!.
term_expander(':-'(use_module(UsedModule,Preds))) :-
    !,
    prolog_load_context(module, Module),
    prolog_load_context(directory,ModulePath),
    get_pred_string(Preds,PredString),
    multi_string_concat(["{:type :use-module :content {:non-lib \"",UsedModule,"\":source-path \"",ModulePath,"\" :in \"",Module,"\" :preds ",PredString,"}}"],Result),
    get_stream(Module,Stream),
    write(Stream,Result),nl(Stream),flush_output(Stream),!.

term_expander(_) :-
    prolog_load_context(module,Module),
    (Module = user; Module = annotations; Module = prolog_analyzer),!.
term_expander(Term) :-
    prolog_load_context(module, Module),
    expand(Term,Module,Result),
    get_stream(Module,Stream),
    write(Stream,Result),nl(Stream),nl(Stream), flush_output(Stream),!.

write_singletons(Clause) :-
    (Clause = ':-'(Head,_); Clause = ':-'(Head); Clause = (Head)),!,
    prolog_load_context(module,Module),
    term_singletons(Clause,Singletons),
    maplist(arg_to_map,Singletons,SingletonsAsStrings),
    join(", ",SingletonsAsStrings,String),
    split(Head,Name,Arity,_,_),
    multi_string_concat(["{:type :singletons :content {:module \"",
                         Module,
                         "\" :name \"",
                         Name,
                         "\" :arity ",
                         Arity,
                         " :singletons [",
                         String,
                        "]}}"],Result),
    get_stream(Module,Stream),
    write(Stream,Result),nl(Stream),nl(Stream), flush_output(Stream),!.

write_singletons(_) :- !.

user:term_expansion(A,Out) :-
    !,
    term_expander(A),
    write_singletons(A),
    ((A = ':-'(module(_));A=':-'(module(_,_))) -> Out=A; Out=[]).

:- multifile user:term_expansion/6.
user:term_expansion(Term, Layout1, Ids, Out, Layout1, [plspec_token|Ids]) :-
    nonmember(plspec_token, Ids),!,
    term_expander(Term),
    ((Term = ':-'(module(_));Term=':-'(module(_,_))) -> Out=Term; Out= Term).

close_orphaned_stream :-
    (retract(edn_stream(_,S)) -> close(S); true).
