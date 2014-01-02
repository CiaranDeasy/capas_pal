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
% Reads a Prolog program from InFileName, and outputs an ML datastructure 
% representing that program to OutFileName.
%%%%%
% Open the files, then read an input, and pass it on to be parsed, along with 
% the filehandle for the rest of the program. Close the files afterwards.
parseProgram( InFileName, OutFileName ) :- 
        open( InFileName, read, InFile ),
        open( OutFileName, write, OutFile ),
        write( OutFile, 'val program = Program( [' ),
        read( InFile, In ), 
		parseNextInput( InFile, In, OutFile ),
        close( InFile ),
        close( OutFile ).

% parseNextInput( +InFile, +In, +OutFile )
% Outputs an ML datastructure representing the "In" term, then reads another 
% term from InFile and calls parseNextInput on that term.
%%%%%
% On the first call, initialise the query list to empty.
parseNextInput( InFile, In, OutFile ) :- parseNextInput( InFile, In, OutFile, [] ).

% If we're at the end of the InFile, then stop.
parseNextInput( _, end_of_file, OutFile, Queries ) :-
        write( OutFile, '] );\n' ),
        write( OutFile, 'val queries = [' ),
        parseQueries( Queries, OutFile ),
		write( OutFile, '];' ).
% If In is a clause...
parseNextInput( InFile, Clause, OutFile, Queries ) :- 
        splitClause( Clause, Head, Body ), 
		!, 
        write( OutFile, 'Clause( ' ),
        parseTerm( Head, OutFile ), 
		write( OutFile, ', ' ),
		splitCompoundTerm( Body, ListBody ),
		write( OutFile, '[' ),
		parsePredList( ListBody, OutFile ), 
		write( OutFile, '] )\n' ),
		read( InFile, In ),
		writeUnlessQueryOrEOF( OutFile, ', \n', In),
		parseNextInput( InFile, In, OutFile, Queries ).
% If In is a query, add it to the list and move on.
parseNextInput( InFile, Query, OutFile, Queries ) :- 
        splitQuery( Query, QueryBody ), 
		!, 
        read( InFile, In ),
		writeUnlessQueryOrEOF( OutFile, ', \n', In),
        parseNextInput( InFile, In, OutFile, [ QueryBody | Queries ] ).
% Or if In is a fact...
parseNextInput( InFile, Fact, OutFile, Queries ) :- 
        write( OutFile, 'Clause( ' ),
        parseTerm( Fact, OutFile ), 
		write( OutFile, ', [] )' ),
		read( InFile, In ),
		writeUnlessQueryOrEOF( OutFile, ', \n', In),
		parseNextInput( InFile, In, OutFile, Queries ).

% parseQueries( +QueryList, +OutFile )
% Writes an ML datastructure representing a list of queries. Does not output
% the enclosing square brackets.
%%%%%
parseQueries( [], _ ).
parseQueries( [X], OutFile ) :-
        parseQuery( X, OutFile ).
parseQueries( [H|T], OutFile ) :-
        parseQuery( H, OutFile ),
		write( OutFile, ', \n' ),
		parseQueries( T, OutFile ).

% parseQuery( +Query, +OutFile )
% Writes an ML datastructure representing a single query.
%%%%%
parseQuery( Body, OutFile ) :-
		write( OutFile, 'Query ( ' ),
		splitCompoundTerm( Body, ListBody ),
		write( OutFile, '[' ),
		parsePredList( ListBody, OutFile ), 
		write( OutFile, '] )' ).

% parseTerm( +Term, +OutFile )
% Writes an ML datastructure to OutFile representing the Term.
%%%%%
parseTerm( Term, OutFile ) :- 
        var( Term ), 
        write( OutFile, 'Variable( "' ), 
		write( OutFile, Term ),
		write( OutFile, '" )' ).
parseTerm( Term, OutFile ) :- 
        write( OutFile, 'Term( Functor( "' ), 
        functor( Term, Functor, Arity ), 
		write_term( OutFile, Functor, [ quoted(true), character_escapes(true) ] ),
		write( OutFile, '" ), ' ),
		parseTermArgs( Term, OutFile, Arity ),
		write( OutFile, ' )' ).

% parsePredList( +Preds, +OutFile )
% Writes an ML datastructure to OutFile representing the list of predicates.
% Doesn't output the enclosing square brackets.
%%%%%
parsePredList( [], _ ).
parsePredList( [X], OutFile ) :-
        parseTerm( X, OutFile ).
parsePredList( [H|T], OutFile ) :- 
        parseTerm( H, OutFile ), 
		write( OutFile, ', ' ),
		parsePredList( T, OutFile ).
		
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
        write( OutFile, '[' ), 
        parseTerm( Arg, OutFile ),
		write( OutFile, ']' ), 
		!.
% First arg.
parseSingleTermArg( Arg, 1, _, OutFile ) :- 
        write( OutFile, '[' ), 
        parseTerm( Arg, OutFile ),
		write( OutFile, ', ' ), 
		!.
% Last arg.
parseSingleTermArg( Arg, Arity, Arity, OutFile ) :- 
        parseTerm( Arg, OutFile ),
		write( OutFile, ']' ), 
		!.
% Other args.
parseSingleTermArg( Arg, _, _, OutFile ) :- 
        parseTerm( Arg, OutFile ),
		write( OutFile, ', ' ), 
		!.

% writeUnlessQueryOrEOF( +OutFile, +Out, +Condition )
% If Condition is a query term or "end_of_file", then this predicate succeeds  
% without writing to OutFile. Otherwise, it writes Out to OutFile and then 
% succeeds.
%%%%%
% EOF case:
writeUnlessQueryOrEOF( _, _, end_of_file ).
% Query case:
writeUnlessQueryOrEOF( _, _, Query ) :- 
    splitQuery( Query, _ ).
% Else do the write:
writeUnlessQueryOrEOF( OutFile, Out, _ ) :-
    write( OutFile, Out ).