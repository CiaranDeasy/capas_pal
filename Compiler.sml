(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the functions that take a Prolog program, represented
as an ML datatype, and output a source ML program that, when run, gives the 
result of running the Prolog program.
*******************************************************************************)

fun compile( inFilename, outFilename ) =
    let val inFile = TextIO.openIn( inFilename ) in
    let val outFile = TextIO.openOut( outFilename ) in
    let val tokenStream = lex( inFile ) in
    let val x as ( program, queries ) = parseStart( tokenStream )
    in
        compilerPrintProgram( outFile, program )
    end end end end
       

and compilerPrintProgram( outStream, Program(clauses) ) = (
        TextIO.output( outStream, "val program = Program([\n" );
        compilerPrintClauseList( outStream, clauses );
        TextIO.output( outStream, "\n" );
        TextIO.output( outStream, "]);\n" );
        TextIO.flushOut( outStream )
    )

and compilerPrintClauseList( outStream, [] ) = ()
  | compilerPrintClauseList( outStream, [clause] ) = (
        compilerPrintClause( outStream, clause );
        TextIO.output( outStream, "\n" )
    )
  | compilerPrintClauseList( outStream, clause::clauses ) = (
        compilerPrintClause( outStream, clause );
        TextIO.output( outStream, ",\n" );
        compilerPrintClauseList( outStream, clauses )
    )

and compilerPrintClause( outStream, Clause( head, body ) ) = (
        TextIO.output( outStream, "Clause( \n" );
        compilerPrintTerm( outStream, head, 1 );
        TextIO.output( outStream, ",\n" );
        compilerPrintIndentation( outStream, 1 );
        TextIO.output( outStream, "[\n" );
        compilerPrintTermList( outStream, body, 2 );
        TextIO.output( outStream, "\n" );
        compilerPrintIndentation( outStream, 1 );
        TextIO.output( outStream, "]\n" );
        TextIO.output( outStream, ")" )
    )
    
and compilerPrintIndentation( outStream, depth ) = 
        if( depth = 0 ) then
            ()
        else
            (
              TextIO.output( outStream, "  " );
              compilerPrintIndentation( outStream, depth - 1 )
            )

and compilerPrintTerm( outStream, Term( Functor( f ), args ), depth ) = (
        compilerPrintIndentation( outStream, depth );
        if( List.length( args ) = 0 ) then (
            TextIO.output( outStream, "Term( Functor( " );
            TextIO.output( outStream, f );
            TextIO.output( outStream, "), [] )" )
        )
        else (
            TextIO.output( outStream, "Term(\n" );
            compilerPrintIndentation( outStream, depth + 1 );
            TextIO.output( outStream, "Functor( \"" );
            TextIO.output( outStream, f );
            TextIO.output( outStream, "\" ),\n" );
            compilerPrintIndentation( outStream, depth + 1 );
            TextIO.output( outStream, "[\n" );
            compilerPrintTermList( outStream, args, depth + 2 );
            compilerPrintIndentation( outStream, depth + 1 );
            TextIO.output( outStream, "]\n" );
            compilerPrintIndentation( outStream, depth );
            TextIO.output( outStream, ")" )
        )
    )
  | compilerPrintTerm( outStream, IntTerm( i ), depth ) = (
        compilerPrintIndentation( outStream, depth );
        TextIO.output( outStream, "IntTerm( " );
        TextIO.output( outStream, ( Int.toString( i ) ) );
        TextIO.output( outStream, " )" )
    )
  | compilerPrintTerm( outStream, FloatTerm( f ), depth ) = (
        compilerPrintIndentation( outStream, depth );
        TextIO.output( outStream, "FloatTerm( " );
        TextIO.output( outStream, ( Real.toString( f ) ) );
        TextIO.output( outStream, " )" )
    )
  | compilerPrintTerm( outStream, Variable( v, s ), depth ) = (
        compilerPrintIndentation( outStream, depth );
        TextIO.output( outStream, "Variable( \"" );
        TextIO.output( outStream, v );
        TextIO.output( outStream, "\", " );
        TextIO.output( outStream, ( Int.toString( s ) ) );
        TextIO.output( outStream, " )" )
    )
        
and compilerPrintTermList( outStream, [], _ ) = ()
  | compilerPrintTermList( outStream, [term], depth ) = (
        compilerPrintTerm( outStream, term, depth );
        TextIO.output( outStream, "\n" )
    )
  | compilerPrintTermList( outStream, term::terms, depth ) = (
        compilerPrintTerm( outStream, term, depth );
        TextIO.output( outStream, ",\n" );
        compilerPrintTermList( outStream, terms, depth )
    )
