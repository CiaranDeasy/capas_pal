% knights(N,Knights) :- Knights is a knight's tour on a NxN chessboard 

knights(N,Knights) :- M is N*N-1,  knights(N,M,[pos(1,1)],Knights).

% closed_knights(N,Knights) :- Knights is a knight's tour on a NxN 
% chessboard which ends at the same square where it begun.

closed_knights(N,Knights) :- 
   knights(N,Knights), Knights = [pos(X,Y)|_], jump(N,pos(X,Y),pos(1,1)). 

% knights(N,M,Visited,Knights) :- the list of squares Visited must be
% extended by M further squares to give the solution Knights of the
% NxN chessboard knight's tour problem. 

member(H, [H|T]).
member(X, [_|T]) :- member(X,T).

knights(_,0,Knights,Knights).
knights(N,M,Visited,Knights) :-
   Visited = [pos(X,Y)|_],
   jump(N,pos(X,Y),pos(U,V)),
   not( member(pos(U,V),Visited) ),
   M1 is M-1,
   knights(N,M1,[pos(U,V)|Visited],Knights).

% jumps on an NxN chessboard from square A/B to C/D
jump(N,pos(A,B),pos(C,D)) :- 
   jump_dist(X,Y), 
   C is A+X, C > 0, Ninc is N+1, C < Ninc,
   D is B+Y, D > 0, D < Ninc.

% jump distances
jump_dist(1,2).
jump_dist(2,1).
jump_dist(2,-1).
jump_dist(1,-2).
jump_dist(-1,-2).
jump_dist(-2,-1).
jump_dist(-2,1).
jump_dist(-1,2).

:- knights( 5, X ).