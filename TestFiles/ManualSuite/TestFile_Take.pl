take( [H|T], H, T ).
take( [H|T], R, [H|S] ) :- take( T, R, S ).

:- take( [1, 2, 3], X, Y ).