% (c) 2009-2019 Lehrstuhl fuer Softwaretechnik und Programmiersprachen,
% Heinrich Heine Universitaet Duesseldorf
% This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html)


:- module(predicate_analysis,[predicate_analysis/5
                             ,predicate_analysis_with_global_sets/5
                             ,info_conjunct/3
                             ,info_disjunct/3
                             ,test_predicate_analysis/0
                             ,max_type_cardinality/2
                             ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extract constraints from syntax elements

extract(Expr,R,X,Y,Op) :-
    extract_symmetric(Expr,R,X1,Y1,Op1),
    symmetric_constraint(Op1,Op2),
    ( Op=Op1,X=X1,Y=Y1
    ; Op=Op2,X=Y1,Y=X1).

extract(integer(I),R,[],[interval:R],constant(irange(I,I))).
extract(value(IV),R,[],[interval:R],constant(irange(I,I))) :- nonvar(IV), IV = int(I). % TO DO: other vals
extract(interval(interval:A,interval:B),R,[interval:A,interval:B],[card:R],interval_card).
extract(member(S:A,elem(S):B),_,[elem(S):B],[S:A],id).
extract(subset(elem(S):A,elem(S):B),_,[elem(S):B],[elem(S):A],id).
extract(subset_strict(elem(S):A,elem(S):B),_,[elem(S):B],[elem(S):A],id).
extract(add(interval:A,interval:B),R,I,O,C) :- addition_constraint(interval:A,interval:B,interval:R,I,O,C).
extract(minus(interval:A,interval:B),R,I,O,C) :- addition_constraint(interval:B,interval:R,interval:A,I,O,C).
extract(minus(interval:A,interval:B),R,[interval:R,interval:B],[interval:A],add).
extract(minus(interval:A,interval:B),R,[interval:A,interval:R],[interval:B],sub).
extract(multiplication(interval:A,interval:B),R,[interval:A,interval:B],[interval:R],mult).
extract(div(interval:A,interval:B),R,[interval:A,interval:B],[interval:R],divrange).
extract(floored_div(interval:A,interval:B),R,[interval:A,interval:B],[interval:R],divrange). % TO DO: provide proper treatment of floored_div
extract(modulo(*,interval:B),R,[interval:B],[interval:R],lt).
extract(modulo(*,*),R,[],[interval:R],constant(irange(0,inf))).
extract(power_of(interval:Base,interval:Exp),R,[interval:Base,interval:Exp],[interval:R],power).
extract(integer_set(Set),R,[],[elem(interval):R],constant(irange(Min,Max))) :-
    compute_integer_set(Set,Min,Max).
extract(bool_set,R,[],[card:R],constant(irange(2,2))).
extract(pow1_subset(*),R,[],[elem(card):R],constant(irange(1,inf))).
extract(fin_subset(A),R,AL,RL,Op) :- extract(pow_subset(A),R,AL,RL,Op).
extract(fin1_subset(A),R,AL,RL,Op) :- extract(pow1_subset(A),R,AL,RL,Op).
extract(partial_function(A,B),R,AL,RL,Op) :- extract(relations(A,B),R,AL,RL,Op).
extract(total_function(A,B),R,AL,RL,Op) :- extract(relations(A,B),R,AL,RL,Op).
extract(partial_injection(A,B),R,AL,RL,Op) :- extract(partial_function(A,B),R,AL,RL,Op).
extract(total_injection(A,B),R,AL,RL,Op) :- extract(total_function(A,B),R,AL,RL,Op).
extract(partial_surjection(A,B),R,AL,RL,Op) :- extract(partial_function(A,B),R,AL,RL,Op).
extract(total_surjection(A,B),R,AL,RL,Op) :- extract(total_function(A,B),R,AL,RL,Op).
extract(total_bijection(A,B),R,AL,RL,Op) :- extract(total_function(A,B),R,AL,RL,Op).
extract(partial_bijection(A,B),R,AL,RL,Op) :- extract(partial_function(A,B),R,AL,RL,Op).
extract(cartesian_product(card:A,card:B),R,[card:A,card:B],[card:R],mult).
extract(cartesian_product(card:A,card:B),R,[card:R,card:A],[card:B],cart_div).
extract(cartesian_product(card:A,card:B),R,[card:R,card:B],[card:A],cart_div).
extract(set_subtraction(elem(S):A,*),R,[elem(S):A],[elem(S):R],id).
extract(image(elem(right(S)):A,*),R,[elem(right(S)):A],[elem(S):R],id).
extract(intersection(S:A,*),R,I,O,C) :- is_subset_extract(S:R,S:A,I,O,C).
extract(intersection(*,S:B),R,I,O,C) :- is_subset_extract(S:R,S:B,I,O,C).
extract(union(elem(S):A,elem(S):B),R,[elem(S):A,elem(S):B],[elem(S):R],union).
extract(union(S:A,*),R,I,O,C) :- is_subset_extract(S:A,S:R,I,O,C).
extract(union(*,S:B),R,I,O,C) :- is_subset_extract(S:B,S:R,I,O,C).
extract(domain_restriction(S:A,*),R,I,O,C) :- is_subset_extract(left(S):R,S:A,I,O,C).
extract(domain_restriction(*,S:B),R,I,O,C) :- is_subset_extract(S:R,S:B,I,O,C).
extract(domain_subtraction(*,S:B),R,I,O,C) :- is_subset_extract(S:R,S:B,I,O,C).
extract(range_restriction(*,S:B),R,I,O,C) :- is_subset_extract(right(S):R,S:B,I,O,C).
extract(range_restriction(S:A,*),R,I,O,C) :- is_subset_extract(S:R,S:A,I,O,C).
extract(range_subtraction(S:A,*),R,I,O,C) :- is_subset_extract(S:R,S:A,I,O,C).
extract(closure(elem(S):A),R,[elem(S):A],[elem(S):R],id).
extract(composition(elem(left(S)):A,*),R,I,O,C) :- is_subset_extract(elem(left(S)):R,elem(left(S)):A,I,O,C).
extract(composition(*,elem(right(S)):B),R,I,O,C) :- is_subset_extract(elem(right(S)):R,elem(right(S)):B,I,O,C).
extract(function(elem(right(S)):A,*),R,[elem(right(S)):A],[S:R],id).
extract(concat(card:A,card:B),R,I,O,C) :- addition_constraint(card:A,card:B,card:R,I,O,C).
extract(concat(Right:A,Right:B),R,[Right:A,Right:B],[Right:R],union) :- Right=elem(right(_)).
extract(concat(S:A,*),R,I,O,C) :- S=elem(right(_)),is_subset_extract(S:A,S:R,I,O,C).
extract(concat(*,S:B),R,I,O,C) :- S=elem(right(_)),is_subset_extract(S:B,S:R,I,O,C).
extract(concat(LI:A,LI:B),R,[LI:A,LI:B],[LI:R],concat_index) :- LI=elem(left(interval)). % TODO: More rules like addition
extract(seq(*),R,[],[elem(elem(left(interval))):R],constant(irange(1,inf))).
extract(seq1(A),R,I,O,C) :- extract(seq(A),R,I,O,C).
extract(seq1(*),R,[],[elem(card):R],constant(irange(1,inf))).
extract(size(A),R,I,O,C) :- extract(card(A),R,I,O,C).

addition_constraint(A,B,R,[A,B],[R],add).
addition_constraint(A,B,R,[R,B],[A],sub).
addition_constraint(A,B,R,[R,A],[B],sub).

is_subset_extract(A,B,I,O,C) :-extract(subset(A,B),_,I,O,C).

extract_symmetric(equal(S:A,S:B),_,[S:A],[S:B],id).
extract_symmetric(subset(card:A,card:B),_,[card:A],[card:B],gte).
extract_symmetric(subset_strict(card:A,card:B),_,[card:A],[card:B],gt).
extract_symmetric(less(interval:A,interval:B),_,[interval:B],[interval:A],lt).
extract_symmetric(less_equal(interval:A,interval:B),_,[interval:B],[interval:A],lte).
extract_symmetric(greater(interval:A,interval:B),_,[interval:B],[interval:A],gt).
extract_symmetric(greater_equal(interval:A,interval:B),_,[interval:B],[interval:A],gte).
extract_symmetric(unary_minus(interval:X),R,[interval:X],[interval:R],negate).
extract_symmetric(card(card:X),R,[card:X],[interval:R],id).
extract_symmetric(cartesian_product(elem(S):A,*),R,[elem(S):A],[elem(left(S)):R],id).
extract_symmetric(cartesian_product(*,elem(S):B),R,[elem(S):B],[elem(right(S)):R],id).
extract_symmetric(couple(S:A,*),R,[S:A],[left(S):R],id).
extract_symmetric(couple(*,S:B),R,[S:B],[right(S):R],id).
extract_symmetric(pow_subset(elem(S):A),R,[elem(S):A],[elem(elem(S)):R],id).
extract_symmetric(pow_subset(card:A),R,[card:A],[elem(card):R],lte).
extract_symmetric(pow1_subset(S),R,[A],[B],C) :- extract_symmetric(pow_subset(S),R,[A],[B],C).
extract_symmetric(seq(A),R,[elem(S):A],[elem(elem(right(S))):R],id).
extract_symmetric(relations(elem(S):A,*),R,[elem(S):A],[elem(elem(left(S))):R],id).
extract_symmetric(relations(*,elem(S):B),R,[elem(S):B],[elem(elem(right(S))):R],id).
extract_symmetric(interval(interval:A,interval:_),R,[interval:A],[elem(interval):R],gte).
extract_symmetric(interval(interval:_,interval:B),R,[interval:B],[elem(interval):R],lte).
extract_symmetric(partial_function(card:A,card:_),R,[card:A],[elem(card):R],lte).
extract_symmetric(total_function(card:A,card:_),R,[card:A],[elem(card):R],id).
extract_symmetric(partial_injection(card:_,card:B),R,[card:B],[elem(card):R],lte).
extract_symmetric(total_injection(card:_,card:B),R,[card:B],[elem(card):R],lte).
extract_symmetric(total_surjection(card:_,card:B),R,[card:B],[elem(card):R],gte).
extract_symmetric(total_bijection(card:A,card:_),R,[card:A],[elem(card):R],id).
extract_symmetric(total_bijection(card:_,card:B),R,[card:B],[elem(card):R],id).
extract_symmetric(domain(elem(left(S)):A),R,[elem(left(S)):A],[elem(S):R],id).
extract_symmetric(range(elem(right(S)):A),R,[elem(right(S)):A],[elem(S):R],id).
extract_symmetric(reverse(elem(left(S)):A),R,[elem(left(S)):A],[elem(right(S)):R],id).
extract_symmetric(reverse(elem(right(S)):A),R,[elem(right(S)):A],[elem(left(S)):R],id).
extract_symmetric(reverse(card:A),R,[card:A],[card:R],id).
extract_symmetric(set_subtraction(card:A,*),R,[card:A],[card:R],gte).
extract_symmetric(closure(elem(left(S)):A),R,[elem(left(S)):A],[elem(left(S)):R],id).
extract_symmetric(closure(elem(right(S)):A),R,[elem(right(S)):A],[elem(right(S)):R],id).
extract_symmetric(closure(card:A),R,[card:A],[card:R],gte).
extract_symmetric(image(card:A,*),R,[card:R],[card:A],gte).
extract_symmetric(first_of_pair(left(S):A),R,[left(S):A],[S:R],id).
extract_symmetric(second_of_pair(right(S):A),R,[right(S):A],[S:R],id).

symmetric_constraint(A,B) :- symmetric_constraint2(A,B),!.
symmetric_constraint(A,B) :- symmetric_constraint2(B,A).
symmetric_constraint2(id,id).
symmetric_constraint2(lt,gt).
symmetric_constraint2(lte,gte).
symmetric_constraint2(negate,negate).
