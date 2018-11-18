:- module(example,[]).
:- use_module(term_expander,[enable_write_out/0]).

:- enable_write_out.

fact(a).
:- dynamic a/1, b/2.
:- fact(a).

a(t).
b(t).
c(t) :- write(c), nl.
d(t) :- write(d), nl.
e(t) :- write(e), nl.
f(t) :- write(f), nl.
g(t) :- write(g), nl.

simple1 :-
    a,b,c.
simple2 :-
    a,b,c,d.

with_brackets :-
    (a,b),(c,d).

with_semicolon :-
    a,b;c,(d1;d2),c2;e,f.

with_semicolon2 :-
    a,(b;c),d.
with_semicolon3 :-
    a,(b;c).

if0 :-
    a -> b.
if1 :-
    a,(b;c) -> d,e;f,g.
if2 :-
    (a -> b;c).
if3(A,B,E) :-
    a(A) ->
        b(B) -> c(t);d(t)
    ;
    e(E) -> f(t);g(t).

foo(X,Y,2) :-
    write(X),
    write(Y),
    bar(X,Y,2).

bar(X,Y,2) :-
    X is Y+2.
bar(X,Y,2) :-
    X is Y-2.

lulu(foo(luu(a,b),mu(c,d)),X) :- X=a.
