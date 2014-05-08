bubblesortpass( [], [], 0 ).
bubblesortpass( [H], [H], 0 ).
bubblesortpass( [H1,H2|T1], [H1|T2], N ) :- 
        H1 < H2, !, bubblesortpass( [H2|T1], T2, N ).
bubblesortpass( [H1,H2|T1], [H2|T2], N ) :- 
        !, bubblesortpass( [H1|T1], T2, M ), N is M+1.

stopOrRecurse( L1, 0, L1 ) :- !.
stopOrRecurse( L1, _, L2 ) :- bubblesort( L1, L2 ).

bubblesort( L1, L3 ) :- bubblesortpass( L1, L2, N ), stopOrRecurse( L2, N, L3 ).

:- bubblesort( [35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0], X ).
%:- bubblesort( [38,23,37,4,19,50,1,34,15,52,18,57,59,53,56,48,54,22,29,17,12,32,31,10,21,25,47,30,45,20,40,9,16,26,3,49,11,8,6,35,33,43,13,14,39,46,5,44,2,7,42,27,28,36,60,41,58,24,55,51 ], X ).