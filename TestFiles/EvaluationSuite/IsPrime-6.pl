% P31 (**) Determine whether a given integer number is prime. 

% is_prime(P) :- P is a prime number
%    (integer) (+)

is_prime(2).
is_prime(3).
is_prime(P) :- P > 3, not( 0 is P mod 2 ), not( has_factor(P,3) ).  

% has_factor(N,L) :- N has an odd factor F >= L.
%    (integer, integer) (+,+)

has_factor(N,L) :- 0 is N mod L.
has_factor(N,L) :- Lsq is L*L, Lsq < N, L2 is L + 2, has_factor(N,L2).

:- is_prime(606449).