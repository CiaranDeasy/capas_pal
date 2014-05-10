ack(0,M,R):- R is M+1,!.
ack(N,0,R):- R1 is N-1,ack(R1,1,R).
ack(N,M,R):- N1 is N-1,M1 is M-1,ack(N,M1,R1),ack(N1,R1,R).

:- ack(1,1,R).
:- ack(1,2,R).
:- ack(2,1,R).
:- ack(2,2,R).
:- ack(2,3,R).
:- ack(3,2,R).
:- ack(3,3,R).