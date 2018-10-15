test(X) :- 
  a;b,c -> 
     write("then"),
     nl, X = b
   ;
     X = c, 
    write("else"),nl.  

