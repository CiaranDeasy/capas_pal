%start :-    write('Write a word='),read(X),nl,
%            name(X,L),permut(L,R),
%            name(Cuv,R),write(Cuv),tab(5),fail.

add(X,L,[X|L]).
add(X,[L|H],[L|R]):- add(X,H,R).
		
permut([],[]).	
permut([L|H],R):- permut(H,R1),add(L,R1,R).

%:- start.
:- add( a, [b,c,d], X ).
:- permut([a,b,c], X).