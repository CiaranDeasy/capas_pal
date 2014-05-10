

append( L, [], L ).
append( [], L, L ).
append( [H|T], L1, [H|L2] ) :- append( T, L1, L2 ).

:- append( [1,2], [3,4], X ), fail.