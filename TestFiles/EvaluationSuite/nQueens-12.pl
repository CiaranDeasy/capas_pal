%% Solver predicates 

exclude_diag(_, [], []).
exclude_diag(Diff, [X|T], [L, R| RE]) :-
    L is X + Diff,
    R is X - Diff,
    Diff1 is Diff + 1,
    exclude_diag(Diff1, T, RE).

% subtract(S, Es, R) deletes all elements in Es from S, and puts the result in R
subtract(Set, [], Set).
subtract(Set, [H|T], Result) :- delete(Set, H, R1), subtract(R1, T, Result).

member( H, [H|T] ).
member( X, [_|T] ) :- member( X, T ).

take( [H|T], H, T ).
take( [H|T], R, [H|S] ) :- take( T, R, S ).

valid(0, _, S, S).
valid(Pos, Range, Sol, R) :-
    exclude_diag(1, Sol, Excl),
    subtract(Range, Excl, Poss),
    member(X, Poss),            % pick one location
    take(Range, X, Rest),     % don't reuse it
    Pos1 is Pos - 1,
    valid(Pos1, Rest, [X|Sol], R).

make_range( 0, [] ).
make_range( N, S) :- H is N, M is N-1, make_range( M, T ), append( T, [H], S ).

append( [], L, L ).
append( [H|T], L, [H|L1] ) :- append( T, L, L1 ).

make_var(N, L) :- length(L, N).

length( [], 0 ).
length( [H|T], N ) :- length( T, M ), N is M+1.
    
queens(Max, Sol) :-
    make_range(Max, Range),
    make_var(Max, Sol),
    valid(Max, Range, [], Sol).

%% Formatting predicates

%format_board(Max, In, Out) :- maplist(format_line(Max), In, Out).

%put_queen(Pos, Pos, _, 'Q').
%put_queen(P1, P2, E, E) :- P1 \= P2.
%format_line(Max, Pos, L) :- make_line(Max, ' ', R), Prev is Pos - 1,
%    take(Prev, R, P, [_|S]), append(P, ['Q'|S], L).

%% Pretty Printer predicates

%print_board(B) :-
%    length(B, L),
%    LL is L * 2 + 1,
%    make_line(LL, '-', LineSep),
%    nl, print_list(LineSep), nl,
%    maplist_(print_line(LineSep), B).

%print_line(LineSep, L) :-
%    print('|'),
%    maplist_(print_square, L), nl,
%    print_list(LineSep), nl.

%print_square(S) :- print(S), print('|').

%% Toplevel predicate
run_queens(N) :- queens(N, S).%, format_board(N, S, B), print_board(B).

:- queens( 12, X ).