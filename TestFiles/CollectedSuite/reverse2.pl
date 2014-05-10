my_reverse(L1,R):- rev(L1,[],R).

rev([],L,L).
rev([H|T],L,M):- rev(T,[H|L],M).

:- my_reverse([m,e,l,a,n,i,e],R).
:- my_reverse([a,[b,1],c],R).
