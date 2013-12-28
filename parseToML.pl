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
% If the first value is a clause, the second and third will be the head and 
% body respectively. Otherwise, fail.
splitClause( X:-Y, X, Y ).

% splitQuery( +Query, -Body )
% If the first value is a query, the second will be the body of the query (ie:
% without the ':-').
splitQuery( :-X, X ).

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
read(File, _), read(File, B),
splitQuery(B, C),
close(File),
%write(Outfile, [Y, I, J, K, A]),
print( C ),
close(OutFile).