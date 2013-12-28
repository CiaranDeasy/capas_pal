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
%%%%%
splitCompoundTerm( (X, Y), [X | T] ) :- 
        !, 
		splitCompoundTerm( Y, T ).
% If the term is not compound, simply return a list containing itself.
splitCompoundTerm( X, [X] ).

% parseProgram( +InFile, +OutFile )
% Reads a Prolog program from InFile, and outputs an ML datastructure 
% representing that program to OutFile.
%%%%%
% Read an input, and pass it on to be parsed, along with the filehandle for the
% rest of the program.
parseProgram( InFile, OutFile ) :- 
        read( InFile, In ), 
		parseNextInput( InFile, In, OutFile ).

% parseNextInput( +InFile, +In, +OutFile )
% Reads the In term, and outputs an ML datastructure representing that term, 
% then calls parseProgram to parse the rest of the InFile.
%%%%%
% If we're at the end of the InFile, then do nothing, and don't ask 
% ParseProgram for anything else.
parseNextInput( _, end_of_file, _ ).
% If In is a clause...
parseNextInput( InFile, Clause, OutFile ) :- 
        splitClause( Clause, Head, Body ), 
        parseTerm( Head, OutFile ), 
		splitCompoundTerm( Body, ListBody ),
		parsePredList( ListBody, OutFile ), 
		parseProgram( InFile, OutFile ).
% If In is a query...
parseNextInput( InFile, Query, OutFile ) :- 
        splitQuery( Query, Body ), 
		parsePredList( Body, OutFile ), 
		parseProgram( InFile, OutFile ).
% Or if In is a fact...
parseNextInput( InFile, Fact, OutFile ) :- 
        parseTerm( Fact, OutFile ), 
		parseProgram( InFile, OutFile ).

% parseTerm( +Term, +OutFile )
% Writes an ML datastructure to OutFile representing the Term.
%%%%%
parseTerm( Term, OutFile ) :- 
        functor( Term, Functor, Arity ), 
		parseTermArgs( Term, OutFile, Arity ).

% parsePredList( +Preds, +OutFile )
% Writes an ML datastructure to OutFile representing the list of predicates.
%%%%%
parsePredList( [H|T], OutFile ) :- 
        parseTerm( H, OutFile ), 
		parsePredList( T, OutFile ).
parsePredList( [], _ ).
		
% parseTermArgs( +Term, +OutFile, +Arity )
% Writes a list of the args of a compound Term to OutFile. Outputs an empty 
% list if Term is atomic.
%%%%%
% Special case: atomic Term.
parseTermArgs( _, OutFile, 0 ) :- 
        write( OutFile, '[]' ).
% Write out each argument, then fail to trigger "arg" to backtrack and give the
% next argument.
parseTermArgs( Term, OutFile, Arity ) :- 
        arg( ArgNo, Term, Value ), 
		parseSingleTermArg( Value, ArgNo, Arity, OutFile ), 
		fail. 
% Give true when "arg" has given all arguments.
parseTermArgs( _, _, _ ).

% parseSingleTermArg( +Term, +Arity, +ArgNo, +OutFile )
%%%%%
% Special case: unary Term.
parseSingleTermArg( Arg, 1, 1, OutFile ) :- 
        write( OutFile, '["' ), 
		write( OutFile, Arg ), 
		write( OutFile, '"]' ), 
		!.
% First arg.
parseSingleTermArg( Arg, 1, _, OutFile ) :- 
        write( OutFile, '["' ), 
		write( OutFile, Arg ), 
		write( OutFile, '", ' ), 
		!.
% Last arg.
parseSingleTermArg( Arg, Arity, Arity, OutFile ) :- 
        write( OutFile, '"' ), 
		write( OutFile, Arg ), 
		write( OutFile, '"]' ), 
		!.
% Other args.
parseSingleTermArg( Arg, _, _, OutFile ) :- 
        write( OutFile, '"' ), 
		write( OutFile, Arg ), 
		write( OutFile, '", ' ).


:- open( "infile.txt", read, InFile ),
open( "outfile.txt", write, OutFile ),
parseProgram( InFile, OutFile ),
close( InFile ),
close( OutFile ).