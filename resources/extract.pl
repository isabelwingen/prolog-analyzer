:- module(extract, []).

%extract_must_write_aux(P,V) :- functor(P,F,N), print(ex_aux(F,N)),nl,fail.
extract_must_write_aux(assign(LHS,_),V) :- maplist(get_id,LHS,Vars), list_to_ord_set(Vars,V).
extract_must_write_aux(assign_single_id(TID,_),[ID]) :- get_id(TID,ID).
extract_must_write_aux(becomes_such(LHS,_),V) :- maplist(get_id,LHS,Vars), list_to_ord_set(Vars,V).
extract_must_write_aux(becomes_element_of(LHS,_),V) :- maplist(get_id,LHS,Vars), list_to_ord_set(Vars,V).
extract_must_write_aux(precondition(_,TBody),MV) :- extract_must_write(TBody,MV).
extract_must_write_aux(assertion(_,TBody),MV) :- extract_must_write(TBody,MV).
extract_must_write_aux(any(_,_,TBody),MV) :- extract_must_write(TBody,MV).
extract_must_write_aux(let(_,_,TBody),MV) :- extract_must_write(TBody,MV).
extract_must_write_aux(lazy_let_subst(_,_,TBody),MV) :- extract_must_write(TBody,MV).
extract_must_write_aux(block(TBody),MV) :- extract_must_write(TBody,MV).
extract_must_write_aux(choice([A]),MV) :- !, extract_must_write(A,MV).
extract_must_write_aux(choice([A|T]),MV) :-
    extract_must_write(A,MV1), extract_must_write_aux(choice(T),MV2), ord_intersection(MV1,MV2,MV).
extract_must_write_aux(if([I]),MV) :- !, get_texpr_expr(I,if_elsif(Cond,TBody)),
    (is_truth(Cond) -> extract_must_write(TBody,MV) ; MV=[]). % we have ELSE skip and then write nothing
extract_must_write_aux(if([I|T]),MV) :- get_texpr_expr(I,if_elsif(_,TBody)),
    extract_must_write(TBody,MV1), extract_must_write_aux(if(T),MV2), ord_intersection(MV1,MV2,MV).
extract_must_write_aux(select([SW]),MV) :- !, get_texpr_expr(SW,select_when(_,TBody)),extract_must_write(TBody,MV).
extract_must_write_aux(select([SW|T]),MV) :-
    get_texpr_expr(SW,select_when(_,TBody)), extract_must_write(TBody,MV1),
    extract_must_write_aux(select(T),MV2),ord_intersection(MV1,MV2,MV).
extract_must_write_aux(select(T,Else),MV) :-
    extract_must_write(Else,MV1),
    extract_must_write_aux(select(T),MV2),ord_intersection(MV1,MV2,MV).
extract_must_write_aux(case(_,[],Else),MV) :- extract_must_write(Else,MV).
extract_must_write_aux(case(E,[C|T],Else),MV) :-
    get_texpr_expr(C,case_or(_,TBody)),
    extract_must_write(TBody,MV1),
    extract_must_write_aux(case(E,T,Else),MV2),ord_intersection(MV1,MV2,MV).
extract_must_write_aux(parallel([A]),MV) :- !, extract_must_write(A,MV).
extract_must_write_aux(parallel([A|T]),MV) :-
    extract_must_write(A,MV1), extract_must_write_aux(parallel(T),MV2), ord_union(MV1,MV2,MV).
extract_must_write_aux(sequence(S),MV) :- !, extract_must_write_aux(parallel(S),MV).
extract_must_write_aux(skip,[]).
extract_must_write_aux(external_subst_call(_,_),[]). % assume there is no guarantee that something is written
extract_must_write_aux(while(_,_,_,_),[]).
extract_must_write_aux(var(Parameters,TBody),MV) :-
   def_get_texpr_ids(Parameters,IDs),
   list_to_ord_set(IDs,MV2),
   extract_must_write(TBody,MV1),
   ord_subtract(MV1,MV2,MV).
extract_must_write_aux(operation_call(TOperation,_,_),MustWriteV) :-
   def_get_texpr_id(TOperation,op(Operation)),
   b_get_read_may_must_write(Operation,MustWriteV,_). %and ideally cache values
extract_must_write_aux(Other,[]) :- print(uncovered_subst_for_must_write(Other)),nl.
