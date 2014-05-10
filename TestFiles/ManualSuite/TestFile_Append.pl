append( [], L, L ).
append( [H|T], L1, [H|L2] ) :- append( T, L1, L2 ).
