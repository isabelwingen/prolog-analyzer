foo(X,Y) :-
    bar(X,Y).

more(X,Y) :-
    foo(X,q),
    bar(z,Y).

bar(a,b). bar(c,d).

% This is a Test
hallo(c,d).

%End
