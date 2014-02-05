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
               | EOF;
               
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
  | eqToken( EOF, EOF ) = true
  | eqToken( _, _ ) = false;

val inStream = TextIO.openIn "C:\\Users\\Ciaran\\Source\\Repos\\capas_pal\\TestFile5.pl";
        
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
    
fun printParserOutput ( program, queries ) = 
        ( printProgram(program); printQueries( queries ) );

val firstLine = String.explode( valOf( TextIO.inputLine inStream ) );

fun lexIdle [] = 
    let val nextLine = TextIO.inputLine inStream
    in
        if( nextLine = NONE )
            then [EOF]
        else
            lexIdle ( String.explode( valOf( nextLine ) ) )
    end
  | lexIdle (x::xs) =
    let fun lexVar( xs ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inStream ) ) )
                  | worker (x::xs) = 
                        if( Char.isAlphaNum x ) 
                            then x :: ( worker xs )
                        else
                            ( remaining := (x::xs);
                            [] )
            in
                ( VARIABLE( String.implode( worker xs ) ) ) 
                        :: ( lexIdle( !remaining ) )
            end end in
    let fun lexAtom( xs ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inStream ) ) )
                  | worker (x::xs) = 
                        if( Char.isAlphaNum x ) 
                            then x :: ( worker xs )
                        else
                            ( remaining := (x::xs);
                            [] )
            in
                ( ATOM( String.implode( worker xs ) ) ) 
                        :: ( lexIdle( !remaining ) )
            end end in
    let fun lexFloat( x::xs, digitList ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inStream ) ) )
                  | worker (x::xs) = 
                        if( Char.isDigit x ) 
                            then x :: ( worker xs )
                        else
                            ( remaining := (x::xs);
                            digitList )
            in
                if( Char.isDigit( hd xs ) )
                    then FLOAT( valOf( Real.fromString( 
                                String.implode( worker xs ) ) ) )
                                  :: ( lexIdle( !remaining ) )
                (* Catch the case where the dot was an end-of-line character. *)
                else
                    ( INT( valOf( Int.fromString( 
                            String.implode( digitList ) ) ) ) ) 
                              :: ( lexIdle( x::xs ) )
            end end in
    let fun lexInt( xs ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inStream ) ) )
                  | worker (x::xs) = 
                        if( Char.isDigit x ) 
                            then x :: ( worker xs )
                        else
                            ( remaining := (x::xs);
                            [] ) in
            let val digitList = worker xs
            in
                if( hd (!remaining) = #"." )
                    then lexFloat( !remaining, digitList )
                else
                    ( INT( valOf( Int.fromString( 
                            String.implode( digitList ) ) ) ) ) 
                              :: ( lexIdle( !remaining ) )
            end end end in
    let fun lexSingleQuoted( xs ) = 
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inStream ) ) )
                  | worker (x::xs) = 
                        if( x = #"\\" ) 
                            then x :: ( hd xs ) :: ( worker ( tl xs ) )
                        else if( x = #"'" )
                            then ( remaining := xs;
                            [#"'"] )
                        else
                            x::( worker xs )
            in
                ( ATOM( String.implode( #"'" :: ( worker xs ) ) ) )
                        :: ( lexIdle( !remaining ) )
            end end in
    let fun lexDoubleQuoted( xs ) = 
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inStream ) ) )
                  | worker (x::xs) = 
                        if( x = #"\\" ) 
                            then x :: ( hd xs ) :: ( worker ( tl xs ) )
                        else if( x = #"\"" )
                            then ( remaining := xs;
                            [#"\""] )
                        else
                            x::( worker xs )
            in
                ( ATOM( String.implode( #"\"" :: ( worker xs ) ) ) )
                        :: ( lexIdle( !remaining ) )
            end end in
    let fun lexIs( x::xs ) = 
            if( x = #"s" andalso Char.isSpace ( hd xs ) ) 
                then IS :: ( lexIdle xs )
            else
                lexAtom( #"i"::x::xs )
    in    
    let fun lexMod( x::xs ) = 
            if( x = #"o" andalso ( hd xs ) = #"d" 
                    andalso Char.isSpace ( hd ( tl xs ) ) ) 
                then MOD :: ( lexIdle ( tl xs ) )
            else
                lexAtom( #"m"::x::xs )
    in    
        if( x = #"i" ) then lexIs( xs )
        else if( x = #"m" ) then lexMod( xs )
        else if( Char.isUpper x ) then lexVar( x::xs )
        else if( Char.isLower x ) then lexAtom( x::xs )
        else if( Char.isDigit x ) then lexInt( x::xs )
        else if( Char.isSpace x ) then lexIdle( xs )
        else if( x = #":" andalso ( hd xs ) = #"-" ) 
            then COLONMINUS :: lexIdle( tl xs )
        else if( x = #"(" ) then LEFTPAREN :: ( lexIdle xs )
        else if( x = #")" ) then RIGHTPAREN :: ( lexIdle xs )
        else if( x = #"," ) then COMMA :: ( lexIdle xs )
        else if( x = #"." ) then DOT :: ( lexIdle xs )
        else if( x = #"[" ) then LEFTSQ :: ( lexIdle xs )
        else if( x = #"]" ) then RIGHTSQ :: ( lexIdle xs )
        else if( x = #"|" ) then PIPE :: ( lexIdle xs )
        else if( x = #"+" ) then PLUS :: ( lexIdle xs )
        else if( x = #"-" ) then MINUS :: ( lexIdle xs )
        else if( x = #"*" ) then MULT :: ( lexIdle xs )
        else if( x = #"/" ) then DIV :: ( lexIdle xs )
        else if( x = #"'" ) then lexSingleQuoted( xs )
        else if( x = #"\"" ) then lexDoubleQuoted( xs )
        else []
    end end end end end end end end;
        
val something = lexIdle firstLine;

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
  | printToken( EOF ) = print "EOF";

fun printTokenStream( [] ) = ()
  | printTokenStream( token::tokens ) = 
        ( printToken token; print ", \n"; printTokenStream( tokens ) );
        
val somethingelse = printTokenStream( something );

(* Returns a term and a list of the remaining tokens. *)
fun parseTail( VARIABLE(v)::tokens ) = ( Variable( v, 0 ), tokens )
  | parseTail( LEFTSQ::tokens ) =
    let val x as (terms, tokens2) = parseTermList( tokens ) in
    let val y as (tail, tokens3) = parseMoreList( tokens2 ) in
    let fun buildList [] = tail
          | buildList (element::elements) = 
                Term( Functor("."), [ element, buildList( elements ) ] )
    in
        ( ( buildList terms ), tokens3 )
    end end end

(* Returns a list of terms and a list of the remaining tokens. *)
and parseArgs( LEFTPAREN::tokens ) =
    let val x as (terms, tokens2) = parseTermList( tokens )
    in
        if( eqToken( hd tokens2, RIGHTPAREN ) )
            then ( terms, ( tl tokens2 ) )
        else
            ( terms, ( tl tokens2 ) ) (*ERROR*)
    end
  | parseArgs( tokens ) = ( [], tokens ) 

(* Returns a term and a list of the remaining tokens. *)
and parseTerm( ATOM(a)::tokens ) = 
    let val x as ( args, tokens2 ) = parseArgs( tokens )
    in 
        ( Term( Functor( a ), args ), tokens2 )
    end
  | parseTerm( LEFTSQ::tokens ) = 
    let val x as (terms, tokens2) = parseTermList( tokens ) in
    let val y as (tail, tokens3) = parseMoreList( tokens2 ) in
    let fun buildList [] = tail
          | buildList (element::elements) = 
                Term( Functor("."), [ element, buildList( elements ) ] )
    in
        ( ( buildList terms ), tokens3 )
    end end end
  | parseTerm( VARIABLE(v)::tokens ) = ( Variable(v, 0), tokens )
  | parseTerm( INT(n)::tokens ) = parseArith( INT(n)::tokens )
  | parseTerm( FLOAT(f)::tokens ) = parseArith( FLOAT(f)::tokens )
  | parseTerm( LEFTPAREN::tokens ) = parseArith( LEFTPAREN::tokens )
(*  | parseTerm( INT(n)::tokens ) = 
        ( Term( Functor( Int.toString(n) ), [] ), tokens )*)

(* Returns a term and a list of the remaining tokens *)
and parseMoreList( RIGHTSQ::tokens ) = ( Term( Functor("[]"), [] ), tokens )
  | parseMoreList( PIPE::tokens ) = 
    let val x as ( term, tokens2 ) = parseTail( tokens )
    in
        if( eqToken( hd tokens2, RIGHTSQ ) )
            then ( term, ( tl tokens2 ) )
        else
            ( term, ( tl tokens2 ) ) (* ERROR *)
    end

(* Returns a list of terms and a list of the remaining tokens *)
and parseMoreTerms( COMMA::tokens ) = parseTermList( tokens )
  | parseMoreTerms( DOT::tokens ) = ( [], DOT::tokens )
  | parseMoreTerms( RIGHTPAREN::tokens ) = ( [], RIGHTPAREN::tokens )
  | parseMoreTerms( RIGHTSQ::tokens ) = ( [], RIGHTSQ::tokens )
  | parseMoreTerms( PIPE::tokens ) = ( [], PIPE::tokens )

(* Returns a list of terms and a list of the remaining tokens *)
and parseTermList( tokens ) = 
    let val x as (term, tokens2) = parseTerm( tokens ) in
    let val y as (isTerm, tokens3) = parseIsTerm( tokens2, term ) in
    let val z as (terms, tokens4) = parseMoreTerms( tokens3 )
    in
        ( (isTerm::terms), tokens4 )
    end end end

(* Returns a list of terms and a list of the remaining tokens. *)
and parseBody( COLONMINUS::tokens ) = parseTermList( tokens )
  | parseBody( DOT::tokens ) = ( [], (DOT::tokens) )

(* Returns a clause and a list of the remaining tokens. *)
and parseClause( tokens ) =
    let val x as (head, tokens2) = parseTerm( tokens ) in
    let val y as (body, tokens3) = parseBody( tokens2 )
    in
        ( Clause( head, body ), tokens3 )
    end end

(* Returns a query and a list of the remaining tokens. *)
and parseQuery( COLONMINUS::tokens ) = 
    let val x as (terms, tokens2) = parseTermList( tokens )
    in
        ( Query( terms ), tokens2 )
    end

(* If the next line is a clause, returns a single-element clause list, an empty
   query list, and a list of the remaining tokens. 
   If the next line is a query, returns an empty clause list, a single-element 
   query list, and a list of the remaining tokens. *)  
and parseLine( COLONMINUS::tokens ) = 
    let val x as ( query, tokens2 ) = parseQuery( COLONMINUS::tokens )
    in
        ( [], [query], tokens2 )
    end
  | parseLine( ATOM(a)::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( ATOM(a)::tokens )
    in
        ( [clause], [], tokens2 )
    end
  | parseLine( VARIABLE(v)::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( VARIABLE(v)::tokens )
    in
        ( [clause], [], tokens2 )
    end
  | parseLine( LEFTSQ::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( LEFTSQ::tokens )
    in
        ( [clause], [], tokens2 )
    end
  | parseLine( INT(n)::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( INT(n)::tokens )
    in
        ( [clause], [], tokens2 )
    end

(* Returns a list of clauses, a list of queries, and a list of the remaining 
   tokens. *)  
and parseMoreLines( ATOM(a)::tokens ) = parseLines( ATOM(a)::tokens )
  | parseMoreLines( VARIABLE(v)::tokens ) = parseLines( VARIABLE(v)::tokens )
  | parseMoreLines( LEFTSQ::tokens ) = parseLines( LEFTSQ::tokens )
  | parseMoreLines( INT(n)::tokens ) = parseLines( INT(n)::tokens )
  | parseMoreLines( COLONMINUS::tokens ) = parseLines( COLONMINUS::tokens )
  | parseMoreLines( EOF::tokens ) = ( [], [], EOF::tokens )

(* Returns a list of clauses, a list of queries, and a list of the remaining 
   tokens. *)
and parseLines( tokens ) =
    let val x as (clause, query, tokens2) = parseLine( tokens )
    in
        if( eqToken( hd tokens2, DOT ) ) 
            then 
                let val y as (clauses, queries, tokens3) = parseMoreLines( tl tokens2 )
                in
                    if( List.null( clause ) )
                        then ( clauses, ( ( hd query ) :: queries ), tokens3 )
                    else
                        ( ( ( hd clause ) :: clauses ), queries, tokens3 )
                end
        else 
            ([], [], tokens) (*ERROR*)
    end
    
(* Returns a term and the remaining tokens. *)
and parseIsTerm( IS::tokens, prevTerm ) = 
    let val x as (nextTerm, tokens2) = parseTerm( tokens ) in
    let val y as (nextIsTerm, tokens3) = parseIsTerm( tokens2, nextTerm )
    in
        ( Term( Functor("is"), [ prevTerm, nextIsTerm ] ), tokens3 )
    end end
  | parseIsTerm( tokens, prevTerm ) = ( prevTerm, tokens )

and parseStart( tokens ) = 
    let val x as (clauses, queries, tokens2) = parseLines( tokens )
    in
        if( eqToken( hd tokens2, EOF ) )
            then (Program(clauses), queries)
        else
            (Program([]), []) (*ERROR*)
    end
    
(* Returns a Term and the remaining tokens. *)
and parseArith( tokens ) = 
    let val x as (arithTerm, tokens2) = parseArithTerm( tokens )
    in
        parseMoreArithTerms( tokens2, arithTerm )
    end
    
(* Takes the current tokens and an already-parsed arithmetic term.
   Returns a Term and the remaining tokens. *)
and parseMoreArithTerms( PLUS::tokens, arithTerm ) =
    let val x as (arithTerms, tokens2) = parseArith( tokens )
    in
        ( Term( Functor("+"), [ arithTerm, arithTerms ] ), tokens2 )
    end
  | parseMoreArithTerms( MINUS::tokens, arithTerm ) =
    let val x as (arithTerms, tokens2) = parseArith( tokens )
    in
        ( Term( Functor("-"), [ arithTerm, arithTerms ] ), tokens2 )
    end
  | parseMoreArithTerms( tokens, arithTerm ) = ( arithTerm, tokens )
    
(* Returns a Term and the remaining tokens. *)
and parseArithTerm( tokens ) = 
    let val x as (factor, tokens2) = parseFactor( tokens )
    in
        parseMoreFactors( tokens2, factor )
    end
    
and parseFactor( LEFTPAREN::tokens ) = 
    let val x as (arith, tokens2) = parseArith( tokens )
    in
        if( eqToken( hd tokens2, RIGHTPAREN ) )
            then ( arith, tl tokens2 )
        else
            ( arith, tl tokens2 ) (*ERROR*)
    end
  | parseFactor( INT(i)::tokens ) = ( IntTerm( i ), tokens )
  | parseFactor( FLOAT(f)::tokens ) = ( FloatTerm( f ), tokens )
  
and parseMoreFactors( MULT::tokens, prevFactor ) = 
    let val x as (nextFactor, tokens2) = parseFactor( tokens )
    in
        parseMoreFactors( tokens2, 
                Term( Functor("*"), [ prevFactor, nextFactor ] ) )
    end
  | parseMoreFactors( DIV::tokens, prevFactor ) = 
    let val x as (nextFactor, tokens2) = parseFactor( tokens )
    in
        parseMoreFactors( tokens2, 
                Term( Functor("/"), [ prevFactor, nextFactor ] ) )
    end
  | parseMoreFactors( MOD::tokens, prevFactor ) = 
    let val x as (nextFactor, tokens2) = parseFactor( tokens )
    in
        parseMoreFactors( tokens2, 
                Term( Functor("%"), [ prevFactor, nextFactor ] ) )
    end
  | parseMoreFactors( tokens, factor ) = ( factor, tokens );
  