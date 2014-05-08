take( [H|T], H, T ).
take( [H|T], R, [H|S] ) :- take( T, R, S ).

perm( [], [] ).
perm( L, [R|T] ) :- take( L, R, S ), perm( S, T ).

sorted( [] ).
sorted( [H] ).
sorted( [H1,H2|T] ) :- H1 < H2, sorted( [H2|T] ).

mySort( L1, L2 ) :- perm( L1, L2 ), sorted( L2 ).

:- mySort( [1,9,2,8,3,7,4,6,5], X ).