%%% 2.1 Predicate piece/1 %%%
piece(['74', [[1,1,0,0,1,0], [0,1,0,1,0,0], [0,1,0,0,1,0], [0,1,0,0,1,1]]]).
piece(['65', [[1,1,0,0,1,1], [1,0,1,1,0,0], [0,0,1,1,0,0], [0,1,0,1,0,1]]]).
piece(['13', [[0,1,0,1,0,1], [1,1,0,1,0,1], [1,1,0,0,1,1], [1,1,0,0,1,0]]]).
piece(['Cc', [[0,0,1,1,0,0], [0,0,1,1,0,0], [0,1,0,0,1,0], [0,0,1,1,0,0]]]).
piece(['98', [[1,1,0,0,1,0], [0,1,0,0,1,0], [0,0,1,1,0,0], [0,0,1,1,0,1]]]).
piece(['02', [[0,0,1,1,0,0], [0,1,0,0,1,1], [1,0,1,1,0,0], [0,0,1,1,0,0]]]).

append( L, [], L ).
append( [], L, L ).
append( [H|T], L1, [H|L2] ) :- append( T, L1, L2 ).

member( H, [H|T] ).
member( A, [H|T] ) :- member( A, T ).

% We enforce that N must not be negative, and return false if it is.
rotate(_, N, _) :- atomic(N), N < 0, !, fail.
% The empty list is always empty.
rotate([], _, []).
% A list after zero rotations is the same list.
rotate(A, 0, A).
% A list under N rotations is a list, with the tail appended to the head, after N-1 rotations.
% Fails if N is uninstantiated.
rotate([H|T], N, B) :- append(T, [H], C), atomic(N), M is N - 1, rotate(C, M, B).

%%% 2.3 Predicate reverse/2 %%%
reverse([], []).
reverse([H|T], A) :- reverse(T, B), append(B, [H], A).

%%% 2.4 Predicate xor/2 %%%
xor(1, 0).
xor(0, 1).

%%% 2.5 Predicate xorlist/2 %%%

% The empty list is its own XOR.
xorlist([], []).
% A list is an xor if the heads are xor and the tails are xor.
xorlist([H1|T1], [H2|T2]) :- xor(H1, H2), xorlist(T1, T2).


%%% 2.6 Numbers in a range %%%

% Return the minimum.
range(Min, Max, Min) :- Min < Max.
% Or increment the minimum.
range(Min, Max, A) :- B is Min + 1, B < Max, range(B, Max, A).
% In either case, test that we are below the max.

% A piece is flipped through the vertical axis if...
flipped([P, [A, B, C, D]], [P, [W, X, Y, Z]]) :-
	% ...the top and bottom are reversed...
    reverse(A, W), reverse(C, Y),
	% ...and the left and right are both exchanged and reversed.
	reverse(B, Z), reverse(D, X).

% Assuming that the input piece is always in canonical orientation.
% For orientations 0, 1, 2 and 3, just rotate anti-clockwise by that amount.
orientation([P, Q], N, [P, R]) :- range(0, 4, N), rotate(Q, N, R).
% For orientations -4, -3, -2, -1, flip the piece and then rotate anti-clockwise by the magnitude of that amount.
orientation([P, Q], N, [P, S]) :- range(-4, 0, N), flipped([P, Q], [P, R]), M is 0-N, rotate(R, M, S).

 % Define a NAND predicate that will be used at the corners.
nand(0, 0).
nand(0, 1).
nand(1, 0).

% compatible/2 compares the edges directly (assuming that the reverse has already been done).
% We nand the corners and xor the rest.
compatible([A1, A2, A3, A4, A5, A6], [B1, B2, B3, B4, B5, B6]) :- 
        nand(A1, B1), xorlist([A2, A3, A4, A5], [B2, B3, B4, B5]), nand(A6, B6).

% compatible/4, as required. Check that the edges are in their respective pieces, reverse one, and then check compatibility.
compatible([_, X], A, [_, Y], B) :- member(A, X), member(B, Y), !, reverse(B, C), compatible(A, C).

% Test cases:
% Two compatible sides.
% Two incompatible sides.
%:- compatible(['36', [[1,1,0,1,0,1], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]]], [1,1,0,1,0,1],
%        ['37', [[1,1,0,1,0,1], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]]], [1,1,0,1,0,1]).

%%% 4.2: Corner compatibility %%%

compatible_corner([_, X], [HA | A], [_, Y], [HB | B], [_, Z], [HC | C]) :- 
% The edges must belong to their corresponding pieces.
        member([HA|A], X), member([HB|B], Y), member([HC|C], Z), !,
% And exactly one edge must have a 1 in its first position.
		1 is HA + HB + HC.
		
% Test cases:
% Compatible corners.
% Incompatible corners.
%:- compatible_corner(['36', [[0,1,0,1,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]]], [0,1,0,1,0,0],
%        ['37', [[1,1,0,1,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]]], [1,1,0,1,0,0],
%        ['38', [[1,1,0,1,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]]], [1,1,0,1,0,0]).

take([H|T], H, T).
take([H|T], P, [H|S]) :- take(T, P, S).

puzzle(PLIST0, [[P0, 0], [P1, 2], [P2, 3], [P3, 1], [P4, 0], [P5, 2]]) :-
			% Bind the output pieces to the input pieces.
			take(PLIST0, P0, PLIST1), !, % Cut to fix the first piece in the first slot, as requested.
			take(PLIST1, P1, PLIST2),
			take(PLIST2, P2, PLIST3),
			take(PLIST3, P3, PLIST4),
			take(PLIST4, P4, PLIST5),
			take(PLIST5, P5, _),
			% Generate orientations of the pieces.
			O0 = 0, % Fix the first piece in orientation 0, as requested.
			orientation(P0, 0, OP0),
			orientation(P1, 2, OP1),
			orientation(P2, 3, OP2),
			orientation(P3, 1, OP3),
			orientation(P4, 0, OP4),
			orientation(P5, 2, OP5),
			% Bind the edges of the oriented pieces.
			OP0 = [_, [OP0E0, OP0E1, OP0E2, OP0E3]],
			OP1 = [_, [OP1E0, OP1E1, OP1E2, OP1E3]],
			OP2 = [_, [OP2E0, OP2E1, OP2E2, OP2E3]],
			OP3 = [_, [OP3E0, OP3E1, OP3E2, OP3E3]],
			OP4 = [_, [OP4E0, OP4E1, OP4E2, OP4E3]],
			OP5 = [_, [OP5E0, OP5E1, OP5E2, OP5E3]],
			% Check the current solution.
		    compatible(OP0, OP0E2, OP1, OP1E0),
			compatible(OP0, OP0E3, OP2, OP2E0),
			compatible(OP1, OP1E3, OP2, OP2E1),
			compatible(OP0, OP0E1, OP3, OP3E0),%
			compatible(OP1, OP1E1, OP3, OP3E3),
			compatible(OP1, OP1E2, OP4, OP4E0),
			compatible(OP2, OP2E2, OP4, OP4E3),
			compatible(OP3, OP3E2, OP4, OP4E1),
			compatible(OP4, OP4E2, OP5, OP5E0),
			compatible(OP2, OP2E3, OP5, OP5E3),
			compatible(OP0, OP0E0, OP5, OP5E2),
			compatible(OP3, OP3E1, OP5, OP5E1),
			compatible_corner(OP0, OP0E3, OP1, OP1E0, OP2, OP2E1),
			compatible_corner(OP0, OP0E2, OP1, OP1E1, OP3, OP3E0),
			compatible_corner(OP2, OP2E2, OP1, OP1E3, OP4, OP4E0),
			compatible_corner(OP3, OP3E3, OP1, OP1E2, OP4, OP4E1),
			compatible_corner(OP5, OP5E0, OP4, OP4E3, OP2, OP2E3),
			compatible_corner(OP5, OP5E1, OP4, OP4E2, OP3, OP3E2),
			compatible_corner(OP5, OP5E2, OP0, OP0E1, OP3, OP3E1),
			compatible_corner(OP5, OP5E3, OP0, OP0E0, OP2, OP2E0).

:- puzzle([['74', [[1,1,0,0,1,0], [0,1,0,1,0,0], [0,1,0,0,1,0], [0,1,0,0,1,1]]],
           ['65', [[1,1,0,0,1,1], [1,0,1,1,0,0], [0,0,1,1,0,0], [0,1,0,1,0,1]]],
           ['13', [[0,1,0,1,0,1], [1,1,0,1,0,1], [1,1,0,0,1,1], [1,1,0,0,1,0]]],
           ['Cc', [[0,0,1,1,0,0], [0,0,1,1,0,0], [0,1,0,0,1,0], [0,0,1,1,0,0]]],
           ['98', [[1,1,0,0,1,0], [0,1,0,0,1,0], [0,0,1,1,0,0], [0,0,1,1,0,1]]],
           ['02', [[0,0,1,1,0,0], [0,1,0,0,1,1], [1,0,1,1,0,0], [0,0,1,1,0,0]]]],
           A ), print(A).

    
    

%:- append( [1, 2], [3, 4], P ), rotate( [1,2,3], 1, Q ), reverse( [1,2,3], R ).