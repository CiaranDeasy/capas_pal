datatype token = ATOM of string
               | VARIABLE of string
               | NUM of int
               | LEFTPAREN
               | RIGHTPAREN
               | COMMA
               | DOT
               | LEFTSQ
               | RIGHTSQ
               | PIPE
               | COLONMINUS
               | EOF;

val inStream = TextIO.openIn "C:\\Users\\Ciaran\\Source\\Repos\\capas_pal\\TestFile2.pl";
        
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
    
fun printAllProg ( clauses, queries ) = 
        ( printClauses( clauses ); printQueries( queries ) );

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
    let fun lexNum( xs ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inStream ) ) )
                  | worker (x::xs) = 
                        if( Char.isDigit x ) 
                            then x :: ( worker xs )
                        else
                            ( remaining := (x::xs);
                            [] )
            in
                ( NUM( valOf( Int.fromString( String.implode(worker xs) ) ) ) ) 
                        :: ( lexIdle( !remaining ) )
            end end in
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
                    (*let val nextLine = TextIO.inputLine inStream
                    in
                        if( nextLine = NONE )
                            then raise 
                        else
                            worker ( String.explode( valOf( nextLine ) ) )*)
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
            end end
    in    
        if( Char.isUpper x ) then lexVar( x::xs )
        else if( Char.isLower x ) then lexAtom( x::xs )
        else if( Char.isDigit x ) then lexNum( x::xs )
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
        else if( x = #"'" ) then lexSingleQuoted( xs )
        else if( x = #"\"" ) then lexDoubleQuoted( xs )
        else []
    end end end end end;
        
val something = lexIdle firstLine;

fun printToken( ATOM(a) ) = ( print "ATOM( "; print a; print " )" )
  | printToken( VARIABLE(v) ) = ( print "VARIABLE( "; print v; print " )" )
  | printToken( NUM(n) ) = 
        ( print "NUM( "; print ( Int.toString( n ) ); print " )" )
  | printToken( LEFTPAREN ) = print "LEFTPAREN"
  | printToken( RIGHTPAREN ) = print "RIGHTPAREN"
  | printToken( COMMA ) = print "COMMA"
  | printToken( DOT ) = print "DOT"
  | printToken( LEFTSQ ) = print "LEFTSQ"
  | printToken( RIGHTSQ ) = print "RIGHTSQ"
  | printToken( PIPE ) = print "PIPE"
  | printToken( COLONMINUS ) = print "COLONMINUS"
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
        if( hd tokens2 = RIGHTPAREN )
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
  | parseTerm( NUM(n)::tokens ) = 
        ( Term( Functor( Int.toString(n) ), [] ), tokens )

(* Returns a term and a list of the remaining tokens *)
and parseMoreList( RIGHTSQ::tokens ) = ( Term( Functor("[]"), [] ), tokens )
  | parseMoreList( PIPE::tokens ) = 
    let val x as ( term, tokens2 ) = parseTail( tokens )
    in
        if( hd tokens2 = RIGHTSQ )
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
    let val y as (terms, tokens3) = parseMoreTerms( tokens2 )
    in
        ( (term::terms), tokens3 )
    end end

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
  | parseLine( NUM(n)::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( NUM(n)::tokens )
    in
        ( [clause], [], tokens2 )
    end

(* Returns a list of clauses, a list of queries, and a list of the remaining 
   tokens. *)  
and parseMoreLines( ATOM(a)::tokens ) = parseLines( ATOM(a)::tokens )
  | parseMoreLines( VARIABLE(v)::tokens ) = parseLines( VARIABLE(v)::tokens )
  | parseMoreLines( LEFTSQ::tokens ) = parseLines( LEFTSQ::tokens )
  | parseMoreLines( NUM(n)::tokens ) = parseLines( NUM(n)::tokens )
  | parseMoreLines( COLONMINUS::tokens ) = parseLines( COLONMINUS::tokens )
  | parseMoreLines( EOF::tokens ) = ( [], [], EOF::tokens )

(* Returns a list of clauses, a list of queries, and a list of the remaining 
   tokens. *)
and parseLines( tokens ) =
    let val x as (clause, query, tokens2) = parseLine( tokens )
    in
        if( hd tokens2 = DOT ) 
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
    end;

fun parseStart( tokens ) = 
    let val x as (clauses, queries, tokens2) = parseLines( tokens )
    in
        if( hd tokens2 ) = EOF then (clauses, queries) else ([], []) (*ERROR*)
    end;
    