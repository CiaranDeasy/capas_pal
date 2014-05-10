t3_ake( [H|T], H, T ).
t3_ake( [H|T], R, [H|S] ) :- t3_ake( T, R, S ).

:- t3_ake( [1, 2, 3], X, Y ), t3_ake( [4, 5, 6], P, Q ).