%start:- write('List of words is: '),read(X),
%        write('Prefix is: '),read(Y),
%        sellist(Y,X,R),
%        write('The words: '),write(R),
%        write('have the prefix '),write(Y),nl.  
	
sellist(P,[],[]).
sellist(P,[W1|RestW],[W1|R]):- prefix(P,W1),sellist(P,RestW,R).
sellist(P,[W1|RestW],R):- sellist(P,RestW,R).

%prefix(X,Y):- name(Y,Ylist),name(X,Xlist),append(Xlist,_,Ylist).

%:- start.