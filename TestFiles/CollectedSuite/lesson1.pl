has(jack,apples).
has(ann,plums).
has(dan,money).
fruit(apples).
fruit(plums).

%:-listing(fruit).
%:-listing(has).
:- has( jack, X ).
:- has( jack, _ ).
:- has( X, apples ), has( Y, plums ).
:- has( X, apples ), has( X, plums ).
:- has(dan,X),fruit(X). 
%:- has(X,Y),not fruit(Y).