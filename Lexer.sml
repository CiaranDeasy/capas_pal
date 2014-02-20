(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the functions that implement the finite state machine
that lexes a Prolog program, converting it from a character stream to a list of 
tokens.
*******************************************************************************)

(* Each "lex" function corresponds to a state in the finite state machine that 
   accepts Prolog syntax. To transition to another state, that state's function
   is called with the list of unlexed characters on the current line, and the 
   input stream that supplies subsequent lines. *)

(* Each wildcard "_" needs a unique variable name, so this mutable value 
   provides unique ascending integers. *)
val wildcardCounter = ref 0;

(* If the character list is empty, fetch a new line from the input stream. If 
   there is no new line, terminated with the EOF token. 
   The transitions are such that only the Idle state can run out of characters 
   on the current line. *)
fun lexIdle( [], inFile ) = 
    let val nextLine = TextIO.inputLine inFile
    in
        if( nextLine = NONE )
            then [EOF]
        else
            lexIdle( ( String.explode( valOf( nextLine ) ) ), inFile )
    end
  | lexIdle( (#"%"::xs), inFile ) = lexIdle( [], inFile )
  | lexIdle( (x::xs), inFile ) =
        if( x = #"i" ) then lexIs( xs, inFile )
        else if( x = #"m" ) then lexMod( xs, inFile )
        else if( Char.isUpper x ) then lexVar( x::xs, inFile )
        else if( Char.isLower x ) then lexAtom( x::xs, inFile )
        else if( Char.isDigit x ) then lexInt( x::xs, inFile )
        else if( Char.isSpace x ) then lexIdle( xs, inFile )
        else if( x = #":" andalso ( hd xs ) = #"-" ) 
            then COLONMINUS :: lexIdle( ( tl xs ), inFile )
        else if( x = #"(" ) then LEFTPAREN :: ( lexIdle( xs, inFile ) )
        else if( x = #")" ) then RIGHTPAREN :: ( lexIdle( xs, inFile ) )
        else if( x = #"," ) then COMMA :: ( lexIdle( xs, inFile ) )
        else if( x = #"." ) then DOT :: ( lexIdle( xs, inFile ) )
        else if( x = #"[" ) then LEFTSQ :: ( lexIdle( xs, inFile ) )
        else if( x = #"]" ) then RIGHTSQ :: ( lexIdle( xs, inFile ) )
        else if( x = #"|" ) then PIPE :: ( lexIdle( xs, inFile ) )
        else if( x = #"+" ) then PLUS :: ( lexIdle( xs, inFile ) )
        else if( x = #"-" ) then MINUS :: ( lexIdle( xs, inFile ) )
        else if( x = #"*" ) then MULT :: ( lexIdle( xs, inFile ) )
        else if( x = #"/" ) then DIV :: ( lexIdle( xs, inFile ) )
        else if( x = #"'" ) then lexSingleQuoted( xs, inFile )
        else if( x = #"\"" ) then lexDoubleQuoted( xs, inFile )
        else if( x = #"_" ) then (
            wildcardCounter := !wildcardCounter + 1;
            VARIABLE( String.concat([ 
                "_", ( Int.toString( !wildcardCounter ) ) 
              ]) ) :: ( lexIdle( xs, inFile ) )
        )
        else []

and lexVar( xs, inFile ) =
    let val remaining = ref [] in
    let fun worker (x::xs) = 
                if( Char.isAlphaNum x ) then 
                    x :: ( worker xs )
                else ( 
                    remaining := (x::xs);
                    [] 
                )
    in
        ( VARIABLE( String.implode( worker xs ) ) ) 
                :: ( lexIdle( ( !remaining ), inFile ) )
    end end

and lexAtom( xs, inFile ) =
    let val remaining = ref [] in
    let fun worker (x::xs) = 
                if( Char.isAlphaNum x ) then 
                    x :: ( worker xs )
                else ( 
                    remaining := (x::xs);
                    []
                )
    in
        ( ATOM( String.implode( worker xs ) ) ) 
            :: ( lexIdle( ( !remaining ), inFile ) )
    end end

and lexInt( xs, inFile ) =
    let val remaining = ref [] in
    let fun worker (x::xs) = 
                if( Char.isDigit x ) then
                    x :: ( worker xs )
                else (
                    remaining := (x::xs);
                    [] 
                ) in
    let val digitList = worker xs
    in
        (* If we see a dot, then this is a float! *)
        if( hd (!remaining) = #"." ) then 
            lexFloat( !remaining, digitList, inFile )
        (* Else it's an int. *)
        else
            ( INT( valOf( Int.fromString( String.implode( digitList ) ) ) ) ) 
                    :: ( lexIdle( ( !remaining ), inFile ) )
            end end end

(* The pre-decimal-point digits of a float will have been lexed by the "Int" 
   state, so lexFloat() takes a digitList argument containing these digits. *)
and lexFloat( x::xs, digitList, inFile ) =
    let val remaining = ref [] in
    let fun worker (x::xs) = 
                if( Char.isDigit x ) 
                    then x :: ( worker xs )
                else ( 
                    remaining := (x::xs);
                    []
                )
    in
        if( Char.isDigit( hd xs ) ) then 
            FLOAT( valOf( Real.fromString( String.implode( 
                    digitList @ ( #"." :: ( worker xs ) ) 
              ) ) ) ) 
                :: ( lexIdle( ( !remaining ), inFile ) )
        (* Catch the case where the dot was an end-of-line character. *)
        else 
            ( INT( valOf( Int.fromString( String.implode( digitList ) ) ) ) ) 
                    :: ( lexIdle( ( x::xs ), inFile ) )
            end end

and lexSingleQuoted( xs, inFile ) = 
    let val remaining = ref [] in
    let fun worker (x::xs) = 
                if( x = #"\\" ) then 
                    x :: ( hd xs ) :: ( worker ( tl xs ) )
                else if( x = #"'" ) then ( 
                    remaining := xs;
                    (* Discard the quote character. *)
                    []
                )
                else
                    x::( worker xs )
    in
        ( ATOM( String.implode( worker xs ) ) )
                :: ( lexIdle( ( !remaining ), inFile ) )
    end end

and lexDoubleQuoted( xs, inFile ) = 
    let val remaining = ref [] in
    let fun worker (x::xs) = 
                if( x = #"\\" ) then 
                    x :: ( hd xs ) :: ( worker ( tl xs ) )
                else if( x = #"\"" ) then (
                    remaining := xs;
                    [#"\""] 
                )
                else
                    x::( worker xs )
    in
        ( ATOM( String.implode( #"\"" :: ( worker xs ) ) ) )
                :: ( lexIdle( ( !remaining ), inFile ) )
    end end

and lexIs( x::xs, inFile ) = 
    if( x = #"s" andalso Char.isSpace ( hd xs ) ) then
        IS :: ( lexIdle( xs, inFile ) )
    else
        lexAtom( #"i"::x::xs, inFile )

and lexMod( x::xs, inFile ) = 
    if( x = #"o" andalso ( hd xs ) = #"d" 
            andalso Char.isSpace ( hd ( tl xs ) ) ) then 
        MOD :: ( lexIdle( ( tl xs ), inFile ) )
    else
        lexAtom( #"m"::x::xs, inFile );

(* Top-level function for the Prolog lexer. Takes an instream for a Prolog 
   source file, and returns a list of tokens. *)
fun lex( inFile ) = lexIdle( [], inFile );
    
