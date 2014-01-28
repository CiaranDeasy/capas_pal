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

val inStream = TextIO.openIn "C:\\Users\\Ciaran\\Source\\Repos\\capas_pal\\TestFile1.pl";

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
        
(*val somethingelse = printTokenStream( something );*)

fun parseTail( VARIABLE(v)::tokens ) = tokens
  | parseTail( LEFTSQ::tokens ) =
    let val tokens2 = parseTermList( tokens )
    in
        parseMoreList( tokens2 )
    end
    
and parseArgs( LEFTPAREN::tokens ) =
    let val tokens2 = parseTermList( tokens )
    in
        if( hd tokens2 = RIGHTPAREN )
            then ( tl tokens2 )
        else
            ( tl tokens2 ) (*ERROR*)
    end
  | parseArgs( tokens ) = tokens
            
and parseTerm( ATOM(a)::tokens ) = parseArgs( tokens )
  | parseTerm( LEFTSQ::tokens ) = 
    let val tokens2 = parseTermList( tokens )
    in
        parseMoreList( tokens2 )
    end
  | parseTerm( VARIABLE(v)::tokens ) = tokens
  | parseTerm( NUM(n)::tokens ) = tokens

and parseMoreList( RIGHTSQ::tokens ) = tokens
  | parseMoreList( PIPE::tokens ) = 
    let val tokens2 = parseTail( tokens )
    in
        if( hd tokens2 = RIGHTSQ )
            then ( tl tokens2 )
        else
            ( tl tokens2 ) (* ERROR *)
    end
  
and parseTermList( tokens ) = 
    let fun parseMoreTerms( COMMA::tokens ) = parseTermList( tokens )
          | parseMoreTerms( DOT::tokens ) = DOT::tokens
          | parseMoreTerms( RIGHTPAREN::tokens ) = RIGHTPAREN::tokens
          | parseMoreTerms( RIGHTSQ::tokens ) = RIGHTSQ::tokens
          | parseMoreTerms( PIPE::tokens ) = PIPE::tokens in
    let val tokens2 = parseTerm( tokens )
    in
        parseMoreTerms( tokens2 )
    end end

and parseBody( COLONMINUS::tokens ) = parseTermList( tokens )
  | parseBody( DOT::tokens ) = DOT::tokens
  | parseBody( tokens ) = ( printTokenStream( tokens ); [] )

and parseClause( tokens ) =
    let val tokens2 = parseTerm( tokens )
    in
        parseBody( tokens2 )
    end

and parseQuery( COLONMINUS::tokens ) = parseTermList( tokens )

and parseLine( COLONMINUS::tokens ) = parseQuery( COLONMINUS::tokens )
  | parseLine( ATOM(a)::tokens ) = parseClause( ATOM(a)::tokens )
  | parseLine( VARIABLE(v)::tokens ) = parseClause( VARIABLE(v)::tokens )
  | parseLine( LEFTSQ::tokens ) = parseClause( LEFTSQ::tokens )
  | parseLine( NUM(n)::tokens ) = parseClause( NUM(n)::tokens )

and parseMoreLines( ATOM(a)::tokens ) = parseLines( ATOM(a)::tokens )
  | parseMoreLines( VARIABLE(v)::tokens ) = parseLines( VARIABLE(v)::tokens )
  | parseMoreLines( LEFTSQ::tokens ) = parseLines( LEFTSQ::tokens )
  | parseMoreLines( NUM(n)::tokens ) = parseLines( NUM(n)::tokens )
  | parseMoreLines( COLONMINUS::tokens ) = parseLines( COLONMINUS::tokens )
  | parseMoreLines( tokens ) = tokens

and parseLines( tokens ) =
    let val tokens2 = parseLine( tokens )
    in
        if( hd tokens2 = DOT ) 
            then parseMoreLines( tl tokens2 )
        else 
            tokens (*ERROR*)
    end;

fun parseStart( tokens ) = 
    let val tokens2 = parseLines( tokens )
    in
        if( hd tokens2 ) = EOF then true else false
    end;
    