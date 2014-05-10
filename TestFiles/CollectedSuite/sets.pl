union([],X,X):-!.
union([X|R],Y,Z):- member(X,Y),union(R,Y,Z),!.
union([X|R],Y,[X|Z]):- union(R,Y,Z).
        
intersect([],X,[]):- !.
intersect([X|R],Y,[X|T]):- member(X,Y),intersect(R,Y,T),!.
intersect([X|R],Y,L):- intersect(R,Y,L).

:- union([a,b,c],[d,e,f],[a,b,c,d,e,f]).
%:- union([a,b,c],[d]).
:- union([a,b,c],[c,d,e],R).
:- intersect([],[a,b,c],[]).
%:- intersect([a,b,c],[c,d],[c]).
:- intersect([a,b,c],[b,c,d],R).
