

append( L, [], L ).
append( [], L, L ).
append( [H|T], L1, [H|L2] ) :- append( T, L1, L2 ).

%%% 2.3 Predicate reverse/2 %%%
reverse([], []).
reverse([H|T], A) :- reverse(T, B), append(B, [H], A).

:- reverse([1,2,3,4,5], A).
%:- reverse(B, [9,8,7,6,5]).
%:- reverse([1,2,3], [3,2,1]).