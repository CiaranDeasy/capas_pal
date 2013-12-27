%foo(X) :- read(X).

%take([H|T], H, T).
%take([H|T], X, [H|S]) :- take(T, X, S).

%:- read(X). take([1,2], 1, [2]). print(X).

parseNext( end_of_file, _ ) :- !.
parseNext( Term, OutFile ) :- splitClause( Term, Term, null ), functor( Term, Functor, Arity ), writeArgs( Term, OutFile, Arity ).  %print "Term.

writeArgs( _, OutFile, 0 ) :- write( OutFile, '[]' ).
writeArgs( Term, OutFile, 1 ) :- write( OutFile, '[' ), arg( 1, Term, Value ), write( OutFile, Value ), write( OutFile, ']' ).
writeArgs( Term, OutFile, Arity ) :- arg( ArgNo, Term, Value ), writeNextArg( Value, ArgNo, Arity, OutFile ), fail. 
writeArgs( _, _, _ ).

writeNextArg( Arg, 1, _, OutFile ) :- write( OutFile, '[' ), write( OutFile, Arg ), write( OutFile, ', ' ), !.
writeNextArg( Arg, Arity, Arity, OutFile ) :- write( OutFile, Arg ), write( OutFile, ']' ), !.
writeNextArg( Arg, _, _, OutFile ) :- write( OutFile, Arg ), write( OutFile, ', ' ).

% splitClause( +Clause, -Head, -Body )
% This predicate takes a clause as input and 
% If the first value is a clause, the second and third will be the head and 
% body respectively.
splitClause( X:-Y, X, Y ).
% If the first value is not a clause, the second will be the term itself and 
% the third will be "null".
splitClause( X, X, null ).

% splitCompoundTerm( +CompoundTerm, -ListOfTerms )
% This predicate takes a compound term, of the form (X1, X2, ..., Xn) and 
% returns a list of the individual terms [X1, X2, ..., Xn].
splitCompoundTerm( (X, Y), [X | T] ) :- !, splitCompoundTerm( Y, T ).
splitCompoundTerm( X, [X] ).

:- open("infile.txt", read, File),
open("outfile.txt", write, OutFile),
read(File, _),
%read(File, Y),
%splitClause(Y, I, J),
%splitCompoundTerm(J, K),
%read(File, Z),
read(File, A),
parseNext( A, OutFile ),
close(File),
%write(Outfile, [Y, I, J, K, A]),
close(OutFile).