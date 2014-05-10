member( H, [H|T] ).
member( X, [H|T] ) :- member( X, T ).

vowel(X):- member(X,[a,e,i,o,u]).

nr_vowel([],0).
nr_vowel([X|T],N):- vowel(X),nr_vowel(T,N1),N is N1+1,!.
nr_vowel([X|T],N):- nr_vowel(T,N).

:-nr_vowel([],X).
:-nr_vowel([a,r,e,d,i],X).
:-nr_vowel([m,r],X).
:-nr_vowel([s,e,e,d],X).
