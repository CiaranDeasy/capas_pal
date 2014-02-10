fun lexIdle [] inFile = 
    let val nextLine = TextIO.inputLine inFile
    in
        if( nextLine = NONE )
            then [EOF]
        else
            lexIdle ( String.explode( valOf( nextLine ) ) ) inFile
    end
  | lexIdle (x::xs) inFile =
    let fun lexVar( xs ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inFile ) ) )
                  | worker (x::xs) = 
                        if( Char.isAlphaNum x ) 
                            then x :: ( worker xs )
                        else
                            ( remaining := (x::xs);
                            [] )
            in
                ( VARIABLE( String.implode( worker xs ) ) ) 
                        :: ( lexIdle( !remaining ) inFile )
            end end in
    let fun lexAtom( xs ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inFile ) ) )
                  | worker (x::xs) = 
                        if( Char.isAlphaNum x ) 
                            then x :: ( worker xs )
                        else
                            ( remaining := (x::xs);
                            [] )
            in
                ( ATOM( String.implode( worker xs ) ) ) 
                        :: ( lexIdle( !remaining ) inFile )
            end end in
    let fun lexFloat( x::xs, digitList ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inFile ) ) )
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
                                  :: ( lexIdle( !remaining ) inFile )
                (* Catch the case where the dot was an end-of-line character. *)
                else
                    ( INT( valOf( Int.fromString( 
                            String.implode( digitList ) ) ) ) ) 
                              :: ( lexIdle( x::xs ) inFile )
            end end in
    let fun lexInt( xs ) =
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inFile ) ) )
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
                              :: ( lexIdle( !remaining ) inFile )
            end end end in
    let fun lexSingleQuoted( xs ) = 
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inFile ) ) )
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
                        :: ( lexIdle( !remaining ) inFile )
            end end in
    let fun lexDoubleQuoted( xs ) = 
            let val remaining = ref [] in
            let fun worker [] = 
                        worker ( String.explode( 
                                valOf( TextIO.inputLine inFile ) ) )
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
                        :: ( lexIdle( !remaining ) inFile )
            end end in
    let fun lexIs( x::xs ) = 
            if( x = #"s" andalso Char.isSpace ( hd xs ) ) 
                then IS :: ( lexIdle xs inFile )
            else
                lexAtom( #"i"::x::xs )
    in    
    let fun lexMod( x::xs ) = 
            if( x = #"o" andalso ( hd xs ) = #"d" 
                    andalso Char.isSpace ( hd ( tl xs ) ) ) 
                then MOD :: ( lexIdle ( tl xs ) inFile )
            else
                lexAtom( #"m"::x::xs )
    in    
        if( x = #"i" ) then lexIs( xs )
        else if( x = #"m" ) then lexMod( xs )
        else if( Char.isUpper x ) then lexVar( x::xs )
        else if( Char.isLower x ) then lexAtom( x::xs )
        else if( Char.isDigit x ) then lexInt( x::xs )
        else if( Char.isSpace x ) then lexIdle( xs ) inFile
        else if( x = #":" andalso ( hd xs ) = #"-" ) 
            then COLONMINUS :: lexIdle( tl xs ) inFile
        else if( x = #"(" ) then LEFTPAREN :: ( lexIdle xs ) inFile
        else if( x = #")" ) then RIGHTPAREN :: ( lexIdle xs ) inFile
        else if( x = #"," ) then COMMA :: ( lexIdle xs ) inFile
        else if( x = #"." ) then DOT :: ( lexIdle xs ) inFile
        else if( x = #"[" ) then LEFTSQ :: ( lexIdle xs ) inFile
        else if( x = #"]" ) then RIGHTSQ :: ( lexIdle xs ) inFile
        else if( x = #"|" ) then PIPE :: ( lexIdle xs ) inFile
        else if( x = #"+" ) then PLUS :: ( lexIdle xs ) inFile
        else if( x = #"-" ) then MINUS :: ( lexIdle xs ) inFile
        else if( x = #"*" ) then MULT :: ( lexIdle xs ) inFile
        else if( x = #"/" ) then DIV :: ( lexIdle xs ) inFile
        else if( x = #"'" ) then lexSingleQuoted( xs )
        else if( x = #"\"" ) then lexDoubleQuoted( xs )
        else []
    end end end end end end end end;

(* Top-level function for the Prolog lexer. Takes an instream for a Prolog 
   source file, and returns a list of tokens. *)
fun lex( inFile ) = lexIdle [] inFile;
    
