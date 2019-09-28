:- module(predicate_analysis, []).

extract(Expr,R,X,Y,Op) :-
    extract_symmetric(Expr,R,X1,Y1,Op1),
    symmetric_constraint(Op1,Op2),
    ( Op=Op1,X=X1,Y=Y1
    ; Op=Op2,X=Y1,Y=X1).

extract(subset(elem(S):A,elem(S):B),_,[elem(S):B],[elem(S):A],id).

extract(domain_restriction(S:A,*),R,I,O,C) :- is_subset_extract(left(S):R,S:A,I,O,C).
extract(domain_restriction(*,S:B),R,I,O,C) :- is_subset_extract(S:R,S:B,I,O,C).
extract(domain_subtraction(*,S:B),R,I,O,C) :- is_subset_extract(S:R,S:B,I,O,C).
extract(range_restriction(*,S:B),R,I,O,C) :- is_subset_extract(right(S):R,S:B,I,O,C).
extract(range_restriction(S:A,*),R,I,O,C) :- is_subset_extract(S:R,S:A,I,O,C).
extract(range_subtraction(S:A,*),R,I,O,C) :- is_subset_extract(S:R,S:A,I,O,C).
is_subset_extract(A,B,I,O,C) :-extract(subset(A,B),_,I,O,C).

extract_symmetric(equal(S:A,S:B),_,[S:A],[S:B],id).
extract_symmetric(subset(card:A,card:B),_,[card:A],[card:B],gte).
symmetric_constraint(A,B) :- symmetric_constraint2(A,B),!.
symmetric_constraint(A,B) :- symmetric_constraint2(B,A).
symmetric_constraint2(id,id).
symmetric_constraint2(lt,gt).
symmetric_constraint2(lte,gte).
symmetric_constraint2(negate,negate).
