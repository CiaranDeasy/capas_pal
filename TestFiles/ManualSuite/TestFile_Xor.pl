%%% 2.4 Predicate xor/2 %%%
xor(1, 0).
xor(0, 1).

%%% 2.5 Predicate xorlist/2 %%%

% The empty list is its own XOR.
xorlist([], []).
% A list is an xor if the heads are xor and the tails are xor.
xorlist([H1|T1], [H2|T2]) :- xor(H1, H2), xorlist(T1, T2).

:- xorlist( [0,1,0,1], [X,Y,Z,A] ).

