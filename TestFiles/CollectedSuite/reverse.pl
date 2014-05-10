append( [], L, L ).
append( [H|T], L1, [H|L2] ) :- append( T, L1, L2 ).

my_reverse([],[]).
my_reverse([H|T],L):- my_reverse(T,R),append(R,[H],L).

:- my_reverse([m,e,l,a,n,i,e],R).
:- my_reverse([a,[b,1],c],R).
