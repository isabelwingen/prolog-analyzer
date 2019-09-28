:- module(tmp, []).

init([_|[]]).
init([H|T]) :-
	  setup_distinguished_table(H, T),
	  init(T).

setup_distinguished_table(_, []).
setup_distinguished_table(X, [H|T]) :-
	  final_states(Final_States),
	  (((member(X, Final_States), \+member(H, Final_States)) | (\+member(X, Final_States), member(H, Final_States))) ->
		     assert(distinguished(X,H,yes)),retractall(table_marked(_)), assert(table_marked(yes))
		; assert(distinguished(X,H,no))),
	  setup_distinguished_table(X,T).
