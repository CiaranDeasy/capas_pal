gcd(X,0,X) :- X > 0.
gcd(X,Y,G) :- Y > 0, Z is X mod Y, gcd(Y,Z,G).

coprime(X,Y) :- gcd(X,Y,1).

totient_phi(1,1) :- !.
totient_phi(M,Phi) :- t_phi(M,Phi,1,0).

% t_phi(M,Phi,K,C) :- Phi = C + N, where N is the number of integers R
%    such that K <= R < M and R is coprime to M.
%    (integer,integer,integer,integer) (+,-,+,+)

t_phi(M,Phi,M,Phi) :- !.
t_phi(M,Phi,K,C) :- 
   K < M, coprime(K,M), !, 
   C1 is C + 1, K1 is K + 1,
   t_phi(M,Phi,K1,C1).
t_phi(M,Phi,K,C) :- 
   K < M, K1 is K + 1,
   t_phi(M,Phi,K1,C).
   
:- totient_phi( 150, X ).