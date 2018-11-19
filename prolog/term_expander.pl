:- module(term_expander,[enable_write_out/0]).

:- multifile term_expansion/2.
:- dynamic write_out/0.
:- public enable_write_out/0.

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
    split(Head,Name,Arity,Arglist),
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
    split(Goal,Name,Arity,Arglist),
    multi_string_concat(["{:goal     \"",Name,"\""],Goal_Elem),
    string_concat(":arity    ",Arity,Arity_Elem),
    create_arglist(Arglist,OtherLineInd,ResArglist),
    append([FirstLineInd/Goal_Elem,OtherLineInd/Arity_Elem],ResArglist,List),
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
    ground(Arg),!,
    arg_to_map(ground,Arg,Map).
arg_to_map(Arg,Map) :-
    var(Arg),!,
    arg_to_map(var,Arg,Map).
arg_to_map(Arg,Map) :-
    compound(Arg),!,
    arg_to_map(compound,Arg,Map).
arg_to_map(Arg,Map) :-
    arg_to_map(any,Arg,Map).
arg_to_map(Type,Term,Map) :-
    term_string(Term,String),
    string_concat("{:term \"", String,R1),
    string_concat(R1, "\" :type :", R2),
    string_concat(R2, Type, R3),
    string_concat(R3, "}",Map).

split(Term,Name,Arity,Arglist) :-
    functor(Term,Name,Arity),
    Term =.. [_|Arglist].

expand(_,term_expander,_) :- !.

expand(':-'(A,B),Module,Stream) :-
    !,
    body_list(B,Body),
    Start = "{:type      :rule\n :content   ",
    rule_to_map(A,Body,Module,Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).


expand(':-'(A),_Module,Stream) :-
    !,
    Start = "{:type      :direct\n :content   ",
    goal_to_map(0,13,A,Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).

expand((C),_Module, Stream) :-
    !,
    Start = "{:type      :fact\n :content   ",
    goal_to_map(0,13,C,Map),
    string_concat(Start,Map,Tmp1),
    string_concat(Tmp1,"}",Tmp2),
    write(Stream,Tmp2),nl(Stream).

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
    (write_out, valid_module(Module) ->
         prolog_load_context(file,File),
         get_clojure_file_name(File,ClojureFile),
         open(ClojureFile,append,Stream),
         expand(A,Module,Stream),
         write(Stream,";; ----------------"),nl(Stream),nl(Stream),
         close(Stream)
    ).

get_clojure_file_name(File,ClojureFile) :-
    string_concat(File,".clj",ClojureFile).

valid_module(Module) :-
    Module \== term_expander,
    Module \== user.
