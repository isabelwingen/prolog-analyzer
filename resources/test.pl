test(X) :- 
  a;b,c -> 
     write("then"),
     nl, X = b
   ;
     X = c, 
    write("else"),nl.  

test2 :-
   write("hallo"),
   b,
   true -> write("t");write("yo");write("fuu"); write("f").


b :- fail.

test3 :-
   true; b -> write("t");write("f").

:- write("a"), write("b").
