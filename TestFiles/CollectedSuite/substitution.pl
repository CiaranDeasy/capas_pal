subst(N,O,[],[]):- !.
subst(N,O,[O|T],[N|R]):- subst(N,O,T,R),!.
subst(N,O,[X|T],[X|R]):- subst(N,O,T,R).

:- subst(new,old,[old,a,old,b,c,old],R).
:- subst(n,o,[a,b,c,d],R).
:- subst(x,y,[y,y,c,[d,y],e],R).
