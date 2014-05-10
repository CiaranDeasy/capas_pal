add(X,L,[X|L]).
add(X,[L|H],[L|R]):- add(X,H,R).

permut([],[]).
permut([L|H],R):- permut(H,R1),add(L,R1,R).

%permutations(L,R):- findall(P,permut(L,P),R).

%:- permutations([1,2],R).
:- add(a,[b,c,d],X).
:- permut([a,b,c],X).
