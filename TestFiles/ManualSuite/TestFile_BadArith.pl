take( [H|T], H, T ) :- 4 is 3--1.
take( [H|T], R, [H|S] ) :- take( T, R, S ).

:- take( [1, 2, 3], X, Y ).