fun compilerPrintProgram( Program(clauses) ) = (
        print "val program = Program([\n";
        compilerPrintClauseList( clauses );
        print "\n";
        print "]);\n"
    )

and compilerPrintClauseList( [] ) = ()
  | compilerPrintClauseList( clause::clauses ) = (
        compilerPrintClause( clause );
        print ",\n";
        compilerPrintClauseList( clauses )
    )

and compilerPrintClause( Clause( head, body ) ) = (
        print "Clause( \n";
        compilerPrintTerm( head, 1 );
        print ",\n";
        compilerPrintIndentation( 1 );
        print "[\n";
        compilerPrintTermList( body, 2 );
        print "\n";
        compilerPrintIndentation( 1 );
        print "]\n";
        print ")"
    )
    
and compilerPrintIndentation( depth ) = 
        if( depth = 0 ) then
            ()
        else
            (
              print "  ";
              compilerPrintIndentation( depth - 1 )
            )

and compilerPrintTerm( Term( Functor( f ), args ), depth ) = (
        compilerPrintIndentation( depth );
        if( List.length( args ) = 0 ) then (
            print "Term( Functor( ";
            print f;
            print "), [] )"
        )
        else (
            print "Term(\n";
            compilerPrintIndentation( depth + 1 );
            print "Functor( ";
            print f;
            print " ),\n";
            compilerPrintIndentation( depth + 1 );
            print "[\n";
            compilerPrintTermList( args, depth + 2 );
            compilerPrintIndentation( depth + 1 );
            print "]\n";
            compilerPrintIndentation( depth );
            print ")"
        )
    )
  | compilerPrintTerm( IntTerm( i ), depth ) = (
        compilerPrintIndentation( depth );
        print "IntTerm( ";
        print ( Int.toString( i ) );
        print " )"
    )
  | compilerPrintTerm( FloatTerm( f ), depth ) = (
        compilerPrintIndentation( depth );
        print "FloatTerm( ";
        print ( Real.toString( f ) );
        print " )"
    )
  | compilerPrintTerm( Variable( v, s ), depth ) = (
        compilerPrintIndentation( depth );
        print "Variable( ";
        print v;
        print ", ";
        print ( Int.toString( s ) );
        print " )"
    )
        
and compilerPrintTermList( [], _ ) = ()
  | compilerPrintTermList( term::terms, depth ) = (
        compilerPrintTerm( term, depth );
        print ",\n";
        compilerPrintTermList( terms, depth )
    )
