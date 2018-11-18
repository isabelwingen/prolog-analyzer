:- module(term_expander,[]).

:- multifile term_expansion/2.

multi_string_concat([H],H) :- !.

multi_string_concat([X,Y|T],Res) :-
    string_concat(X,Y,R),
    multi_string_concat([R|T],Res).

join(_,[X],X) :- !.
join(Sep,[X,Y|T],Res) :-
    string_concat(X,Sep,XKomma),
    string_concat(XKomma,Y,XKommaY),
    join(Sep,[XKommaY|T],Res).

mapcat(Goal,[],_,"[]",_) :- !.
mapcat(Goal,List,Sep,Res,Break) :-
    maplist(Goal,List,List2),
    join(Sep,List2,Join),
    (Break == true ->
         multi_string_concat(["[\n",Join,"]"],Res)
     ;
         multi_string_concat(["[",Join,"]"],Res)
    ).
create_map([],Indent,Map) :-
    string_concat(Indent,"{}",Map).
create_map([H|L],Indent,Map) :-
    multi_string_concat([Indent,"{",H],Start),
    string_concat(Indent," ",NewIndent),
    create_map(L,NewIndent,Start,Map).
create_map([],Indent,R,Map) :-
    string_concat(R,"}",Map).
create_map([H|T],Indent,In,Map) :-
    multi_string_concat([In,"\n",Indent,H],Out),
    create_map(T,Indent,Out,Map).


rule_to_map(Head,Body,Module,Map) :-
    split(Head,Name,Arity,Arglist),
    mapcat(arg_to_map,Arglist," ", ArglistString,false),
    mapcat(goal_to_map(12),Body,"\n",BodyString,true),
    string_concat(":name     ",Name,Goal_Elem),
    string_concat(":module   ",Module,Module_Elem),
    string_concat(":arity    ",Arity,Arity_Elem),
    string_concat(":arglist  ",ArglistString,Arglist_Elem),
    string_concat(":body     ",BodyString,Body_Elem),
    create_map([Goal_Elem,Module_Elem,Arity_Elem,Arglist_Elem,Body_Elem],"",Map).

goal_to_map(Goal,Map) :-
    goal_to_map(8,Goal,Map).

goal_to_map(I,[H|T],String) :-
    indent(I,Indent),
    string_concat("\n",Indent,Sep),
    mapcat(goal_to_map, [H|T], Sep, String, false).
goal_to_map(I,or(Arglist),Map) :-
    indent(I,Indent),
    length(Arglist,Arity),
    mapcat(goal_to_map,Arglist," ",ArglistString,false),
    string_concat(":goal     ","or",Goal_Elem),
    string_concat(":arity    ",Arity,Arity_Elem),
    string_concat(":arglist  ",ArglistString,Arglist_Elem),
    create_map([Goal_Elem,Arity_Elem,Arglist_Elem],Indent,Map).

goal_to_map(I,Goal,Map) :-
    indent(I,Indent),
    split(Goal,Name,Arity,Arglist),
    mapcat(arg_to_map,Arglist," ", ArglistString,false),
    string_concat(":goal     ",Name,Goal_Elem),
    string_concat(":arity    ",Arity,Arity_Elem),
    string_concat(":arglist  ",ArglistString,Arglist_Elem),
    create_map([Goal_Elem,Arity_Elem,Arglist_Elem],Indent,Map).

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
    arg_to_map(any,Arg,Map).

arg_to_map(Type,Term,Map) :-
    term_string(Term,String),
    string_concat("{:term ", String,R1),
    string_concat(R1, " :type ", R2),
    string_concat(R2, Type, R3),
    string_concat(R3, "}",Map).



indent(0,"") :- !.
indent(N,R) :-
    M is N-1,
    indent(M,S),
    string_concat(S," ",R).

split(Term,Name,Arity,Arglist) :-
    functor(Term,Name,Arity),
    Term =.. [_|Arglist].

expand(':-'(A,B),Module) :-
    !,
    body_list(B,Body),
    rule_to_map(A,Body,Module,Map),
    write(Map),nl.

expand(':-'(A),_Module) :-
    !,
    write("Direct: "), write(A),nl.

expand((C),_Module) :-
    !,
    write("Fact: "), write(C), nl.

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

transform(Body,if(LeftList,RightList)) :-
    Body =.. [(->),Left,Right],
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
    expand(A,Module),
    write("----------------"),nl,nl.

