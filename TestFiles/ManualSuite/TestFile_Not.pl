foo(X) :- not( bar( X ) ).
bar(2).

:- foo(3).
:- foo(2).