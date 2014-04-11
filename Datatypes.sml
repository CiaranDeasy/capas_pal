(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains datatypes used by the Prolog interpreter/compiler, and
some utility functions for those datatypes.
*******************************************************************************)

(* The token datatype represents tokens generated by the Prolog lexer. *)
datatype token = ATOM of string
               | VARIABLE of string
               | INT of int
               | FLOAT of real
               | LEFTPAREN
               | RIGHTPAREN
               | COMMA
               | DOT
               | LEFTSQ
               | RIGHTSQ
               | PIPE
               | COLONMINUS
               | IS
               | PLUS
               | MINUS
               | MULT
               | DIV
               | MOD
               | LESS
               | GREATER
               | EQUALS
               | CUT
               | EOF;

(* The following datatypes represent a parsed Prolog program. *)
datatype functor_t = Functor of string;
datatype term_t = Term of functor_t * term_t list
                | Variable of string * int
                | IntTerm of int
                | FloatTerm of real;
datatype clause_t = Clause of term_t * term_t list;
datatype program_t = Program of clause_t list;
datatype query_t = Query of term_t list;
datatype binding_t = Binding of term_t * term_t;
datatype unifier_t = Unifier of binding_t list;
          

(*******************************************************************************
The following are the equality tests for the above datatypes.
*******************************************************************************)

(* Equality test for tokens. *)
fun eqToken( ATOM(a1), ATOM(a2) ) = ( a1 = a2 )
  | eqToken( VARIABLE(v1), VARIABLE(v2) ) = ( v1 = v2 )
  | eqToken( INT(i1), INT(i2) ) = ( i1 = i2 )
  | eqToken( FLOAT(f1), FLOAT(f2) ) = ( Real.==( f1, f2 ) )
  | eqToken( LEFTPAREN, LEFTPAREN ) = true
  | eqToken( RIGHTPAREN, RIGHTPAREN ) = true
  | eqToken( COMMA, COMMA ) = true
  | eqToken( DOT, DOT ) = true
  | eqToken( LEFTSQ, LEFTSQ ) = true
  | eqToken( RIGHTSQ, RIGHTSQ ) = true
  | eqToken( PIPE, PIPE ) = true
  | eqToken( COLONMINUS, COLONMINUS ) = true
  | eqToken( IS, IS ) = true
  | eqToken( PLUS, PLUS ) = true
  | eqToken( MINUS, MINUS ) = true
  | eqToken( MULT, MULT ) = true
  | eqToken( DIV, DIV ) = true
  | eqToken( MOD, MOD ) = true
  | eqToken( LESS, LESS ) = true
  | eqToken( GREATER, GREATER ) = true
  | eqToken( EQUALS, EQUALS ) = true
  | eqToken( CUT, CUT ) = true
  | eqToken( EOF, EOF ) = true
  | eqToken( _, _ ) = false;

(* Equality test for Terms *)
fun eqTerm( Term( f1, args1 ), Term( f2, args2 ) ) = 
        ( f1 = f2 ) andalso ( eqOrderedList eqTerm ( args1, args2 ) )
  | eqTerm( Variable( v1, s1 ), Variable( v2, s2 ) ) =
        ( v1 = v2 ) andalso ( s1 = s2 )
  | eqTerm( IntTerm( i1 ), IntTerm( i2 ) ) = ( i1 = i2 )
  | eqTerm( FloatTerm( f1 ), FloatTerm( f2 ) ) = Real.==( f1, f2 )
  | eqTerm( term1, term2 ) = false;

(* Equality test for Bindings *)
fun eqBinding( Binding( term1A, term1B ), Binding( term2A, term2B ) ) = 
        ( eqTerm( term1A, term2A ) andalso eqTerm( term1B, term2B ) )
          orelse ( eqTerm( term1A, term2B ) andalso eqTerm( term1B, term2A ) );

(* Equality test for Unifiers *)
fun eqUnifier ( Unifier(xs), Unifier(ys) ) = 
        eqUnorderedList eqBinding ( xs, ys );

(* Equality test for Clauses *)
fun eqClause( Clause(head1, body1), Clause(head2, body2) ) = 
        eqTerm( head1, head2 ) andalso eqOrderedList eqTerm ( body1, body2 );


(*******************************************************************************
The following are functions for printing out the datatypes above.
*******************************************************************************)

(* Helper function: takes a binding and returns another Binding with the 
   elements in reverse order. *)
fun flipBinding ( Binding( x, y ) ) = Binding( y, x );

fun printTerm ( Variable(name, scope) ) = ( 
        if( not(scope = 0) andalso not(scope = 1) )
            then ( print ( Int.toString( scope ) ); print "_" )
        else
            ();
        print name )
  | printTerm ( Term( Functor( func ), terms ) ) = 
    let fun printListTerm( Term(_, [term, Variable(v,s)] ) ) = 
                ( printTerm( term ); print "|"; printTerm( Variable(v,s) ) )
          | printListTerm( Term(_, [term, Term( Functor( f ), args )] ) ) = (
                printTerm( term );
                if( f = "." )
                    then ( print ", "; 
                           printListTerm( Term( Functor( f ), args ) ) )
                else (*if( f = "[]" )*)
                    ()
                )
    in
        ( 
            if( func = "." )
                then ( print "["; 
                       printListTerm( Term( Functor( func ), terms ) );
                       print "]" )
            else
                ( print func; 
                if( not( null terms ) )
                    then ( 
                        print "(";
                        printTerms terms;
                        print ")"
                    )
                else
                    () )
        )
    end
  | printTerm( IntTerm( i ) ) = print ( Int.toString( i ) )
  | printTerm( FloatTerm( f ) ) = print ( Real.toString( f ) )

and printTerms [] = ()
  | printTerms [term] = printTerm term
  | printTerms (term::terms) = ( 
        printTerm term; 
        print ", ";
        printTerms terms
    );
        
fun printTermRaw( Variable(name, scope) ) = ( 
        if( not(scope = 0) andalso not(scope = 1) )
            then ( print ( Int.toString( scope ) ); print "_" )
        else
            ();
        print name )
  | printTermRaw ( Term( Functor( func ), terms ) ) = 
        ( 
                ( print func; 
                if( not( null terms ) )
                    then ( 
                        print "(";
                        printTermsRaw terms;
                        print ")"
                    )
                else
                    () )
        )
  | printTermRaw( IntTerm( i ) ) = print ( Int.toString( i ) )
  | printTermRaw( FloatTerm( f ) ) = print ( Real.toString( f ) )

and printTermsRaw [] = ()
  | printTermsRaw [term] = printTermRaw term
  | printTermsRaw (term::terms) = ( 
        printTermRaw term; 
        print ", ";
        printTermsRaw terms
    );

fun printQuery ( Query(terms) ) = 
    let fun printTerms [] = ()
          | printTerms [term] = printTerm term
          | printTerms (term::terms) = ( 
                printTerm ( term ); 
                print ", ";
                printTerms( terms ) 
            ) in
        printTerms terms 
    end;
    
fun printAllBindings [] = ()
  | printAllBindings (binding::bindings) = 
    let fun printBinding ( Binding( term1, term2 ) ) = (
        printTerm ( term1 );
        print " = ";
        printTerm term2
       )
    in
        (
            printBinding binding;
            print "\n";
            printAllBindings bindings
        )
    end;
    
(* Only outputs variable-to-term Bindings, and only for variables in the 
   original query, ie: scope "1". *)
fun printCoreBindings [] = ()
  | printCoreBindings (binding::bindings) = 
    let fun printBinding( Binding( Variable( _, _ ), Variable( _, _ ) ) ) = ()
          | printBinding( Binding( Variable( name, scope ), term ) ) =
        if( scope = 1 ) then (
            printTerm ( Variable( name, scope ) );
            print " = ";
            printTerm ( term );
            print "\n"
        )
        else 
            ()
          | printBinding binding = printBinding ( flipBinding binding ) 
    in
        (
            printBinding binding;
            printCoreBindings bindings
        )
    end;
    
fun printUnifier ( Unifier(bindings) ) = printCoreBindings bindings;
fun printFullUnifier( Unifier(bindings) ) = printAllBindings bindings;

fun printClauses [] = ()
  | printClauses ((Clause(head,body))::clauses) = (
        printTerm( head );
        print " :- ";
        printTerms( body );
        print "\n";
        printClauses( clauses )
    );
    
fun printQueries [] = ()
  | printQueries (query::queries) = (
        printQuery( query );
        print "\n";
        printQueries( queries )
    );
    
fun printProgram( Program(xs) ) = printClauses(xs);

fun printToken( ATOM(a) ) = ( print "ATOM( "; print a; print " )" )
  | printToken( VARIABLE(v) ) = ( print "VARIABLE( "; print v; print " )" )
  | printToken( INT(n) ) = 
        ( print "INT( "; print ( Int.toString( n ) ); print " )" )
  | printToken( FLOAT(f) ) = 
        ( print "FLOAT( "; print ( Real.toString( f ) ); print " )" )
  | printToken( LEFTPAREN ) = print "LEFTPAREN"
  | printToken( RIGHTPAREN ) = print "RIGHTPAREN"
  | printToken( COMMA ) = print "COMMA"
  | printToken( DOT ) = print "DOT"
  | printToken( LEFTSQ ) = print "LEFTSQ"
  | printToken( RIGHTSQ ) = print "RIGHTSQ"
  | printToken( PIPE ) = print "PIPE"
  | printToken( COLONMINUS ) = print "COLONMINUS"
  | printToken( IS ) = print "IS"
  | printToken( PLUS ) = print "PLUS"
  | printToken( MINUS ) = print "MINUS"
  | printToken( MULT ) = print "MULT"
  | printToken( DIV ) = print "DIV"
  | printToken( MOD ) = print "MOD"
  | printToken( LESS ) = print "LESS"
  | printToken( GREATER ) = print "GREATER"
  | printToken( EQUALS ) = print "EQUALS"
  | printToken( CUT ) = print "CUT"
  | printToken( EOF ) = print "EOF";

fun printTokenStream( [] ) = ()
  | printTokenStream( token::tokens ) = 
        ( printToken token; print ", \n"; printTokenStream( tokens ) );

(*******************************************************************************
The following are functions related to the datatypes above.
*******************************************************************************)

(* Takes a Query and updates all variables occurring in it to have scope "1". *)
fun scopeQuery( Query(xs) ) = 
    let fun scopeTerms [] = []
          | scopeTerms (term::terms) = 
            let fun scopeTerm ( Term( f, args ) ) = 
                        Term( f, ( scopeTerms args ) )
                  | scopeTerm ( Variable( v, _ ) ) = 
                        Variable( v, 1 )
                  | scopeTerm ( IntTerm(i) ) = IntTerm(i)
                  | scopeTerm ( FloatTerm(f) ) = FloatTerm(f)
            in
                ( scopeTerm( term ) ) :: ( scopeTerms( terms ) )
            end
    in
        Query( scopeTerms( xs ) )                        
    end;
    
(* Takes two term-to-variable bindings, and applies the first to the second as a
   substitution, returning the updated second binding. Returns the binding 
   unchanged if either binding is variable-to-variable. *)
(* If either Binding is purely variables, then there is no change. *)
fun substitute( Binding( Variable( _, _ ), Variable( _, _ ) ), b2 ) = b2
  | substitute( b1, Binding( Variable( v1, s1 ), Variable( v2, s2 ) ) ) = 
        Binding( Variable( v1, s1 ), Variable( v2, s2 ) )
(* Mirror bindings if the variable comes before the term. *)
  | substitute( Binding( Variable( v1, s1 ), t1 ), b2 ) = 
        substitute( Binding( t1, Variable( v1, s1 ) ), b2 )
  | substitute( b1, Binding( Variable( v2, s2 ), t2 ) ) = 
        substitute( b1, Binding( t2, Variable( v2, s2 ) ) )
(* No substitution if the target is an IntTerm or a FloatTerm binding *)
  | substitute( b1, Binding( IntTerm(i), Variable(v,s) ) ) = 
        Binding( IntTerm(i), Variable(v,s) )
  | substitute( b1, Binding( FloatTerm(f), Variable(v,s) ) ) = 
        Binding( FloatTerm(f), Variable(v,s) )
(* Main version when the target is a Term: *)
  | substitute( Binding( term, Variable( v1, s1 ) ),
            Binding( Term( f2, args2 ), Variable( v2, s2 ) ) ) =
    let fun worker( Term( f, args ) ) = 
            let fun iterateArgs( [] ) = []
                  | iterateArgs( arg::remaining ) = 
                        ( worker( arg ) ) :: ( iterateArgs( remaining ) )
            in
                Term( f, ( iterateArgs( args ) ) )
            end
          | worker( Variable( v, s ) ) = 
                if( eqTerm( Variable( v, s ), Variable( v1, s1 ) ) )
                    then term
                else Variable( v, s )
          | worker( IntTerm(i) ) = IntTerm(i)
          | worker( FloatTerm(f) ) = FloatTerm(f)
    in
        Binding( ( worker( Term( f2, args2 ) ) ), Variable( v2, s2 ) )
    end;

(* Takes a consistent Unifier with no term-to-term Bindings. Expands Bindings 
   containing Terms by replacing Variables with Terms according to other 
   Bindings in the Unifier. *)
fun substituteUnifier( Unifier(xs) ) = 
    let fun addBinding( Binding( x, y ), bindings ) = 
        let fun firstRound( newBinding, [] ) = newBinding
              | firstRound( newBinding, ( nextBinding::remaining ) ) = 
                    firstRound( substitute(nextBinding, newBinding), remaining )
        in
        let fun secondRound( newBinding, [] ) = [ newBinding ]
              | secondRound( newBinding, ( nextBinding::remaining ) ) = 
                    ( substitute( newBinding, nextBinding ) )::
                            ( secondRound( newBinding, remaining ) )
        in
            secondRound( firstRound( Binding( x, y ), bindings ), bindings )
        end end in
    let fun worker( [], ys ) = Unifier(ys)
          | worker( (x::xs), ys ) = worker( xs, ( addBinding( x, ys ) ) )
    in
        worker( xs, [] )
    end end;

(* Takes a Unifier and returns a Unifier containing the same Bindings, excluding
   those which don't include a variable. *)
fun cleanUnifier( Unifier(xs) ) =
    let fun worker( [] ) = []
          | worker( ( Binding( Term( _, _ ), Term( _, _ ) ) )::bindings ) = 
                worker( bindings )
          | worker( ( Binding( IntTerm( _ ), IntTerm( _ ) ) )::bindings ) = 
                worker( bindings )
          | worker( ( Binding( FloatTerm( _ ), FloatTerm( _ ) ) )::bindings ) = 
                worker( bindings )
          | worker( ( Binding( term1, term2 ) )::bindings ) = 
                ( Binding( term1, term2 ) )::( worker( bindings ) )
    in
        Unifier( worker xs )
    end;
