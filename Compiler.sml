

fun outputTermDatatypeFlat( out, Term( Functor( f ), args ) ) = (
        if( List.length( args ) = 0 ) then (
            TextIO.output( out, "Term( Functor( \"" );
            TextIO.output( out, f );
            TextIO.output( out, "\" ), [] )" )
        )
        else (
            TextIO.output( out, "Term( Functor( \"" );
            TextIO.output( out, f );
            TextIO.output( out, "\" ), [ " );
            outputTermListDatatypeFlat( out, args );
            TextIO.output( out, " ] )" )
        )
    )
  | outputTermDatatypeFlat( out, IntTerm( i ) ) = (
        TextIO.output( out, "IntTerm( " );
        TextIO.output( out, ( Int.toString( i ) ) );
        TextIO.output( out, " )" )
    )
  | outputTermDatatypeFlat( out, FloatTerm( f ) ) = (
        TextIO.output( out, "FloatTerm( " );
        TextIO.output( out, ( Real.toString( f ) ) );
        TextIO.output( out, " )" )
    )
  | outputTermDatatypeFlat( out, Variable( v, s ) ) = (
        TextIO.output( out, "Variable( \"" );
        TextIO.output( out, v );
        TextIO.output( out, "\", " );
        if( s = 0 ) then
            TextIO.output( out, "scope" )
        else
            TextIO.output( out, Int.toString( s ) );
        TextIO.output( out, " )" )
    )

and outputTermListDatatypeFlat( out, [] ) = ()
  | outputTermListDatatypeFlat( out, [term] ) = 
            outputTermDatatypeFlat( out, term )
  | outputTermListDatatypeFlat( out, term::terms ) = (
        outputTermDatatypeFlat( out, term );
        TextIO.output( out, ", " );
        outputTermListDatatypeFlat( out, terms )
    );
    
(* Takes a term and outputs the name of a predicate to call to execute that 
   function. Needs to detect special predicates. *)
fun outputPredicateCall( out, name, arity ) = 
        if( name = "<" andalso arity = 2 ) then
            TextIO.output( out, "specialPredicateLess" )
        else if( name = ">" andalso arity = 2 ) then
            TextIO.output( out, "specialPredicateGreater" )
        else if( name = "print" andalso arity = 1 ) then
            TextIO.output( out, "specialPredicatePrint1" )
        else if( name = "=" andalso arity = 2 ) then
            TextIO.output( out, "specialPredicateEquals" )
        else if( name = "fail" andalso arity = 0 ) then
            TextIO.output( out, "specialPredicateFail" )
        else (
            TextIO.output( out, name );
            TextIO.output( out, "_" );
            TextIO.output( out, Int.toString( arity ) )
        );

(* Outputs code that imports the necessary source files, and defines a scope 
   counter. Every output program needs this preamble. *)
fun outputPreamble( out ) = (
        TextIO.output( out, "val _ = (\n" );
        TextIO.output( out, "    use \"Utility.sml\";\n" );
        TextIO.output( out, "    use \"Datatypes.sml\";\n" );
        TextIO.output( out, "    use \"Unification.sml\";\n" );
        TextIO.output( out, "    use \"SpecialPredicates.sml\"\n" );
        TextIO.output( out, ")\n" );
        TextIO.output( out, "\n" );
        TextIO.output( out, "val scopeCounter = ref 2;\n" );
        TextIO.output( out, "fun getScope() = (\n" );
        TextIO.output( out, "        scopeCounter := !scopeCounter + 1;\n" );
        TextIO.output( out, "        !scopeCounter\n" );
        TextIO.output( out, ")\n" );
        TextIO.output(out,  "\n" )
)
    
(* Takes a list of clauses representing a predicate. Examines the head clause 
   and returns the name of the predicate. *)
fun getPredName( Clause( Term( Functor( name ), _ ), _ )::_ ) = name;

(* Takes a list of clauses representing a predicate. Examines the head clause 
   and returns the number of arguments that the predicate takes. *)
fun getPredArity( Clause( Term( _, args ), _ )::_ ) = List.length( args );

(* Takes a clause and a list of clauses. Returns two lists of clauses, where the
   first contains the single input clause and all clauses in the input list that
   have the same head, and the second contains the remaining clauses in the 
   input list. *)
fun getMatchingClauses( clause, clauses ) =
    let val x as ( Clause( Term( Functor( mainHead ), mainArgs ), _ ) ) = clause
        val arity = List.length( mainArgs )
        val matching = ref [clause]
        fun worker( [] ) = []
          | worker( clause::clauses ) = 
            let val x as ( Clause( Term( Functor( head ), args ), _ ) ) = clause
            in
                if( ( head = mainHead ) andalso 
                        ( List.length( args ) = arity ) ) then (
                    matching := clause::(!matching);
                    worker( clauses )
                )
                else
                    clause :: worker( clauses )
            end
        val nonMatching = worker( clauses )
    in
        ( List.rev(!matching), nonMatching )
    end;

(* Takes a list of clauses in a program. Outputs functions that implement the 
   predicates. *)
fun compilePredicates( _, [] ) = ()
  | compilePredicates( out, clause::clauses ) = 
    let val x as (predPatterns, remainingClauses) = 
            getMatchingClauses( clause, clauses )
    in (
        compilePredicate( out, predPatterns ); 
        if( List.null( clauses ) ) then
            TextIO.output( out, ";\n\n" )
        else (
            TextIO.output( out, "\n\n" );
            compilePredicates( out, remainingClauses )
        )
    )
    end
        
(* Takes a list of clauses representing a predicate. Outputs functions that 
   implement the predicate. *)
and compilePredicate( out, clauses ) =
    let val name = getPredName( clauses )
        val arity = getPredArity( clauses )
        val patterns = List.length( clauses )
        fun outputArgs( this ) = ( 
                TextIO.output( out, "arg" );
                TextIO.output( out, Int.toString( this ) );
                TextIO.output( out, ", " );
                if( this < arity ) then outputArgs( this+1 ) else ()
        )
        fun outputContinuations( this ) = (
                TextIO.output( out, "m" );
                TextIO.output( out, Int.toString( this ) );
                TextIO.output( out, "() = " );
                TextIO.output( out, name );
                TextIO.output( out, "_" );
                TextIO.output( out, Int.toString( arity ) );
                TextIO.output( out, "_patt_" );
                TextIO.output( out, Int.toString( this ) );
                TextIO.output( out, "( uni, " );
                outputArgs( 1 );
                TextIO.output( out, "succ, " );
                if( this < patterns ) then (
                    TextIO.output( out, "m" );
                    TextIO.output( out, Int.toString( this + 1 ) );
                    TextIO.output( out, ", fail )\n" );
                    TextIO.output( out, "        and " );
                    outputContinuations( this + 1 )
                )
                else (
                    TextIO.output( out, "fail, fail ) in\n" )
                )
        )
    in (
        TextIO.output( out, "and " );
        TextIO.output( out, name );
        TextIO.output( out, "_" );
        TextIO.output( out, Int.toString( arity ) );
        TextIO.output( out, "( uni, " );
        outputArgs( 1 );
        TextIO.output( out, "succ, fail ) = \n" );
        
        TextIO.output( out, "    let fun " );
        outputContinuations( 1 );
        TextIO.output( out, "        m1()\n" );
        (*DEBUG*)
        (*TextIO.output( out, "        ( print \"" );
        TextIO.output( out, name );
        TextIO.output( out, "_" );
        TextIO.output( out, Int.toString( arity ) );
        TextIO.output( out, "\\n\"; m1() )\n" );*)
        (*/DEBUG*)
        TextIO.output( out, "    end\n" );
        TextIO.output( out, "\n" );
        
        compilePatterns( out, clauses );
        
        TextIO.flushOut( out )
    )
    end
    
and compilePatterns( out, clauses ) = 
    let fun worker( [], _ ) = ()
          | worker( clause::clauses, pattNum ) = (
                compilePattern( out, clause, pattNum );
                if( List.null( clauses ) ) then 
                    ()
                else (
                    TextIO.output( out, "\n\n" );
                    worker( clauses, pattNum+1 )
                )
        )
    in
        worker( clauses, 1 )
    end
    
and compilePattern( out, clause, pattNum ) = 
    let val x as ( Clause( Term( Functor( head ), args ), body ) ) = clause
        val arity = List.length( args )
        fun outputArgs( this ) = ( 
                TextIO.output( out, "arg" );
                TextIO.output( out, Int.toString( this ) );
                TextIO.output( out, ", " );
                if( this < arity ) then outputArgs( this+1 ) else ()
        )
        fun outputArgMatchers() = 
            let fun worker( [], _ ) = ()
                  | worker( arg::args, num ) = ( 
                        TextIO.output( out, "    let val x as (succ" );
                        TextIO.output( out, Int.toString( num ) );
                        TextIO.output( out, ", uni" );
                        TextIO.output( out, Int.toString( num ) );
                        TextIO.output( out, ") = unify( uni" );
                        TextIO.output( out, Int.toString( num-1 ) );
                        TextIO.output( out, ", Binding( arg" );
                        TextIO.output( out, Int.toString( num ) );
                        TextIO.output( out, ", " );
                        outputTermDatatypeFlat( out, arg );
                        TextIO.output( out, " ) )\n" );
                        
                        TextIO.output( out, "    in if( not(succ" );
                        TextIO.output( out, Int.toString( num ) );
                        TextIO.output( out, ") ) then fail() else\n" );
                        worker( args, num+1 )
                    )
            in
                worker( args, 1 )
            end
        fun outputBodyContinuations() = 
            let fun worker( Term( Functor( f ), args )::terms, num ) = (
                        if( ( f = "!" ) andalso List.length( args ) = 0 ) then 
                            outputBodyContinuationCut( num, terms ) 
                        else (
                        TextIO.output( out, "m" );
                        TextIO.output( out, Int.toString( num ) );
                        TextIO.output( out, "( uni, fail" );
                        TextIO.output( out, Int.toString( num ) );
                        TextIO.output( out, " ) = " );
                        outputPredicateCall( out, f, ( List.length( args ) ) );
                        TextIO.output( out, "( uni, " );
                        if( List.length( args ) > 0 ) then (
                            outputTermListDatatypeFlat( out, args );
                            TextIO.output( out, ", " )
                        ) else ();
                        (* If this is not the last term of the body, the success
                           continuation is the next term of the body. *)
                        if( num < ( List.length( body ) ) ) then (
                            TextIO.output( out, "m" );
                            TextIO.output( out, Int.toString( num+1 ) );
                            TextIO.output( out, ", fail" );
                            TextIO.output( out, Int.toString( num ) );
                            TextIO.output( out, " )\n" );
                            TextIO.output( out, "            and " );
                            worker( terms, num+1 )
                        )
                        else (
                            TextIO.output( out, "succ, fail" );
                            TextIO.output( out, Int.toString( num ) );
                            TextIO.output( out, " ) in\n" )
                        )
                        )
                )
                and outputBodyContinuationCut( num, terms ) = (
                        TextIO.output( out, "m" );
                        TextIO.output( out, Int.toString( num ) );
                        TextIO.output( out, "( uni, fail" );
                        TextIO.output( out, Int.toString( num ) );
                        TextIO.output( out, " ) = specialPredicateCut( uni, " );
                        if( List.null( terms ) ) then (
                            TextIO.output( out, "succ, globalFail ) in\n" )
                        )
                        else (
                            TextIO.output( out, "m" );
                            TextIO.output( out, Int.toString( num + 1 ) );
                            TextIO.output( out, ", globalFail )\n" );
                            TextIO.output( out, "            and " );
                            worker( terms, num+1 )
                        )
                )
            in
                worker( body, 1 )
            end
        fun outputEnds( _, 0 ) = ()
          | outputEnds( out, ends ) = (
                TextIO.output( out, " end" );
                outputEnds( out, ends-1 )
            )
    in (
        TextIO.output( out, "and " );
        TextIO.output( out, head );
        TextIO.output( out, "_" );
        TextIO.output( out, Int.toString( arity ) );
        TextIO.output( out, "_patt_" );
        TextIO.output( out, Int.toString( pattNum ) );
        TextIO.output( out, "( uni0, " );
        outputArgs( 1 );
        TextIO.output( out, "succ, fail, globalFail ) = \n" );
        TextIO.output( out, "    let val scope = getScope() in\n" );
        outputArgMatchers();
        if( List.null( body ) ) then (
            TextIO.output( out, "        succ( uni" );
            TextIO.output( out, Int.toString( arity ) );
            TextIO.output( out, ", fail )\n" )
        )
        else (
            TextIO.output( out, "        let fun " );
            outputBodyContinuations();
            TextIO.output( out, "            m1( uni" );
            TextIO.output( out, Int.toString( arity ) );
            TextIO.output( out, ", fail )\n" );
            TextIO.output( out, "        end\n" )
        );
        TextIO.output( out, "    end" );
        outputEnds( out, arity )
    )
    end;
    
fun compileQuery( out, Query( terms ), num ) = 
    let fun outputContinuations( Term( Functor(f), args )::terms, termNum ) = (
                if( ( f = "!" ) andalso List.length( args ) = 0 ) then (
                    outputContinuationCut( termNum, terms )
                )
                else (
                TextIO.output( out, "m" );
                TextIO.output( out, Int.toString( termNum ) );
                TextIO.output( out, "( uni, fail" );
                TextIO.output( out, Int.toString( termNum ) );
                TextIO.output( out, " ) = " );
                outputPredicateCall( out, f, ( List.length( args ) ) );
                TextIO.output( out, "( uni, " );
                outputTermListDatatypeFlat( out, args );
                TextIO.output( out, ", " );
                (* If this is the last query term... *)
                if( List.null( terms ) ) then (
                    TextIO.output( out, "succ, fail" );
                    TextIO.output( out, Int.toString( termNum ) );
                    TextIO.output( out, " ) in\n" )
                )
                else (
                    TextIO.output( out, "m" );
                    TextIO.output( out, Int.toString( termNum+1 ) );
                    TextIO.output( out, ", fail" );
                    TextIO.output( out, Int.toString( termNum ) );
                    TextIO.output( out, " )\n" );
                    TextIO.output( out, "        and " );
                    outputContinuations( terms, termNum+1 )
                )
                )
        )
        and outputContinuationCut( termNum, terms ) = (
                TextIO.output( out, "m" );
                TextIO.output( out, Int.toString( termNum ) );
                TextIO.output( out, "( uni, fail" );
                TextIO.output( out, Int.toString( termNum ) );
                TextIO.output( out, " ) = specialPredicateCut( uni, " );
                if( List.null( terms ) ) then (
                    TextIO.output( out, "succ, fail ) in\n" )
                )
                else (
                    TextIO.output( out, "m" );
                    TextIO.output( out, Int.toString( termNum + 1 ) );
                    TextIO.output( out, ", fail )\n" );
                    TextIO.output( out, "        and " );
                    outputContinuations( terms, termNum+1 )
                )
        )
    in
        TextIO.output( out, "fun query_" );
        TextIO.output( out, Int.toString( num ) );
        TextIO.output( out, "() = \n" );
        TextIO.output( out, "    let fun succ( uni, fail ) = (\n" );
        TextIO.output( out, "                print( \"success\\n\" );\n" );
        TextIO.output( out, "                printUnifier( " );
        TextIO.output( out, "substituteUnifier( cleanUnifier( uni ) ) )\n" );
        TextIO.output( out, "            )\n" );
        TextIO.output( out, "        fun fail() = print( \"failure\\n\" ) \n" );
        TextIO.output( out, "        fun " );
        outputContinuations( terms, 1 );
        TextIO.output( out, "        m1( Unifier([]), fail )\n" );
        TextIO.output( out, "    end;\n\n" )
    end;

fun compileQueries( out, queries ) = 
    let fun worker( [], _ ) = ()
          | worker( query::queries, num ) = (
                compileQuery( out, query, num );
                worker( queries, num+1 )
            )
    in
        worker( queries, 1 )
    end;
    
(* Outputs the execute() function that executes the queries in the program, and 
   a call to the execute function. Outputs nothing if there are no queries. *)
fun outputPostamble( out, [] ) = ()
  | outputPostamble( out, queries ) = 
    let fun outputQueries( query::queries, num ) = (
                TextIO.output( out, "        query_" );
                TextIO.output( out, Int.toString( num ) );
                TextIO.output( out, "()" );
                if( List.null( queries ) ) then
                    TextIO.output( out, "\n" )
                else (
                    TextIO.output( out, ";\n" );
                    outputQueries( queries, num+1 )
                )
        )
    in
        TextIO.output( out, "fun execute() = (\n" );
        outputQueries( queries, 1 );
        TextIO.output( out, ")\n\n" );
        TextIO.output( out, "val _ = execute()\n" )
    end
    
fun compileProgram( outFilename, Program( clauses ), queries ) = 
    let val out = TextIO.openOut( outFilename )
    in (
        outputPreamble( out );
        TextIO.flushOut( out );
        compilePredicates( out, clauses );
        TextIO.flushOut( out );
        compileQueries( out, queries );
        TextIO.flushOut( out );
        outputPostamble( out, queries );
        TextIO.flushOut( out );
        TextIO.closeOut( out )
    )
    end;
