myMod(X, Y, Z, A) :- Z is X mod Y, A = !.

take( [H|T], H, T ).
take( [H|T], R, [H|S] ) :- take( T, R, S ).

takeCut( [H|T], H, T ) :- !.
takeCut( [H|T], R, [H|S] ) :- takeCut( T, R, S ).

neg( 2 ).

:- myMod(5, -3, Z, !), take( [ !, !, ! ], X, Y ), take( [1,2,3], P, Q ), neg( P ).
:- takeCut( [1,2,3], P, Q ), neg( P ).
:- myMod(5, -3, Z, !), take( [ !, !, ! ], X, Y ), take( [1,2,3], P, Q ), !, neg( P ).