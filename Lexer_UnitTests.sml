(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the unit tests for functions defined in Lexer.sml
*******************************************************************************)

fun unitTestLexer() = (
  let val dummyStream = TextIO.openIn( "TestFiles\\DummyFile.txt" )
  in
    UnitTester.printResultPoly "lexVar 1"
      ( lexVar( [ #"D", #"e", #"r", #"p", #"l", #"\n" ], dummyStream ) )
      [ VARIABLE( "Derpl" ), EOF ]
      ( eqOrderedList eqToken );
    (* With numbers *)
    UnitTester.printResultPoly "lexVar 2"
      ( lexVar( [ #"D", #"e", #"1", #"p", #"l", #"\n" ], dummyStream ) )
      [ VARIABLE( "De1pl" ), EOF ]
      ( eqOrderedList eqToken );
      
    UnitTester.printResultPoly "lexAtom 1"
      ( lexAtom( [ #"d", #"e", #"r", #"p", #"l", #"\n" ], dummyStream ) )
      [ ATOM( "derpl" ), EOF ]
      ( eqOrderedList eqToken );
    (* With numbers *)
    UnitTester.printResultPoly "lexAtom 2"
      ( lexAtom( [ #"d", #"e", #"5", #"p", #"l", #"\n" ], dummyStream ) )
      [ ATOM( "de5pl" ), EOF ]
      ( eqOrderedList eqToken );
      
    (* Integer *)
    UnitTester.printResultPoly "lexInt 1"
      ( lexInt( [ #"1", #"2", #"3", #"4", #"5", #"\n" ], dummyStream ) )
      [ INT( 12345 ), EOF ]
      ( eqOrderedList eqToken );
    (* Float *)
    UnitTester.printResultPoly "lexInt 2"
      ( lexInt( [ #"1", #"2", #".", #"4", #"5", #"\n" ], dummyStream ) )
      [ FLOAT( 12.45 ), EOF ]
      ( eqOrderedList eqToken );
    (* Integer and end-of-line *)
    UnitTester.printResultPoly "lexInt 3"
      ( lexInt( [ #"1", #"2", #"3", #"4", #".", #"\n" ], dummyStream ) )
      [ INT( 1234 ), DOT, EOF ]
      ( eqOrderedList eqToken );
      
    (* Float *)
    UnitTester.printResultPoly "lexFloat 1"
      ( lexFloat( [ #".", #"4", #"5", #"\n" ], [ #"1", #"2" ], dummyStream ) )
      [ FLOAT( 12.45 ), EOF ]
      ( eqOrderedList eqToken );
    (* Int and end-of-line *)
    UnitTester.printResultPoly "lexFloat 2"
      ( lexFloat( [ #".", #"\n" ], [ #"1", #"2" ], dummyStream ) )
      [ INT( 12 ), DOT, EOF ]
      ( eqOrderedList eqToken );
      
    (* Simple case. *)
    UnitTester.printResultPoly "lexSingleQuoted 1"
      ( lexSingleQuoted( 
        [ #"d", #"e", #"r", #"p", #"l", #"'", #"\n" ], dummyStream ) )
      [ ATOM( "derpl" ), EOF ]
      ( eqOrderedList eqToken );
    (* Awkward characters. *)
    UnitTester.printResultPoly "lexSingleQuoted 2"
      ( lexSingleQuoted( 
        [ #"d", #"\\", #"'", #"p", #"%", #"&", #"\\", #"n", #"'", #"\n" ], 
          dummyStream ) )
      [ ATOM( "d\\'p%&\\n" ), EOF ]
      ( eqOrderedList eqToken );
      
    (* Simple case. *)
    UnitTester.printResultPoly "lexDoubleQuoted 1"
      ( lexDoubleQuoted( 
        [ #"d", #"e", #"r", #"p", #"l", #"\"", #"\n" ], dummyStream ) )
      [ ATOM( "\"derpl\"" ), EOF ]
      ( eqOrderedList eqToken );
    (* Awkward characters. *)
    UnitTester.printResultPoly "lexDoubleQuoted 2"
      ( lexDoubleQuoted( 
        [ #"d", #"\\", #"\"", #"p", #"%", #"&", #"\\", #"n", #"\"", #"\n" ], 
          dummyStream ) )
      [ ATOM( "\"d\\\"p%&\\n\"" ), EOF ]
      ( eqOrderedList eqToken );
      
    (* Matching case. *)
    UnitTester.printResultPoly "lexIs 1"
      ( lexIs( [ #"s", #"\n" ], dummyStream ) )
      [ IS, EOF ]
      ( eqOrderedList eqToken );
    (* Non-matching case. *)
    UnitTester.printResultPoly "lexIs 2"
      ( lexIs( [ #"n", #"b", #"r", #"e", #"d", #"\n" ], dummyStream ) )
      [ ATOM( "inbred" ), EOF ]
      ( eqOrderedList eqToken );
    (* Prefix *)
    UnitTester.printResultPoly "lexIs 3"
      ( lexIs( [ #"s", #"s", #"u", #"n", #"\n" ], dummyStream ) )
      [ ATOM( "issun" ), EOF ]
      ( eqOrderedList eqToken );
    (* Uppercase S *)
    UnitTester.printResultPoly "lexIs 4"
      ( lexIs( [ #"S", #"\n" ], dummyStream ) )
      [ ATOM( "iS" ), EOF ]
      ( eqOrderedList eqToken );
      
    (* Matching case. *)
    UnitTester.printResultPoly "lexMod 1"
      ( lexMod( [ #"o", #"d", #"\n" ], dummyStream ) )
      [ MOD, EOF ]
      ( eqOrderedList eqToken );
    (* Non-matching case. *)
    UnitTester.printResultPoly "lexMod 2"
      ( lexMod( [ #"i", #"n", #"e", #"c", #"r", #"a", #"f", #"t", #"\n" ], 
        dummyStream ) )
      [ ATOM( "minecraft" ), EOF ]
      ( eqOrderedList eqToken );
    (* Prefix *)
    UnitTester.printResultPoly "lexMod 3"
      ( lexMod( [ #"o", #"d", #"u", #"l", #"o", #"\n" ], dummyStream ) )
      [ ATOM( "modulo" ), EOF ]
      ( eqOrderedList eqToken );
    (* Uppercase D *)
    UnitTester.printResultPoly "lexMod 4"
      ( lexMod( [ #"o", #"D", #"\n" ], dummyStream ) )
      [ ATOM( "moD" ), EOF ]
      ( eqOrderedList eqToken );
      
    (* is *)
    UnitTester.printResultPoly "lexIdle 1"
      ( lexIdle( [ #"i", #"s", #"\n" ], dummyStream ) )
      [ IS, EOF ]
      ( eqOrderedList eqToken );
    (* mod *)
    UnitTester.printResultPoly "lexIdle 2"
      ( lexIdle( [ #"m", #"o", #"d", #"\n" ], dummyStream ) )
      [ MOD, EOF ]
      ( eqOrderedList eqToken );
    (* Variable *)
    UnitTester.printResultPoly "lexIdle 3"
      ( lexIdle( [ #"A", #"y", #"l", #"a", #"\n" ], dummyStream ) )
      [ VARIABLE( "Ayla" ), EOF ]
      ( eqOrderedList eqToken );
    (* atom *)
    UnitTester.printResultPoly "lexIdle 4"
      ( lexIdle( [ #"d", #"e", #"r", #"p", #"l", #"\n" ], dummyStream ) )
      [ ATOM( "derpl" ), EOF ]
      ( eqOrderedList eqToken );
    (* int *)
    UnitTester.printResultPoly "lexIdle 5"
      ( lexIdle( [ #"1", #"2", #"3", #"\n" ], dummyStream ) )
      [ INT( 123 ), EOF ]
      ( eqOrderedList eqToken );
    (* float *)
    UnitTester.printResultPoly "lexIdle 6"
      ( lexIdle( [ #"6", #"5", #".", #"5", #"3", #"5", #"\n" ], dummyStream ) )
      [ FLOAT( 65.535 ), EOF ]
      ( eqOrderedList eqToken );
    (* space *)
    UnitTester.printResultPoly "lexIdle 7"
      ( lexIdle( [ #" ", #"\t", #"\n" ], dummyStream ) )
      [ EOF ]
      ( eqOrderedList eqToken );
    (* colon-minus *)
    UnitTester.printResultPoly "lexIdle 8"
      ( lexIdle( [ #":", #"-", #"\n" ], dummyStream ) )
      [ COLONMINUS, EOF ]
      ( eqOrderedList eqToken );
    (* brackets *)
    UnitTester.printResultPoly "lexIdle 9"
      ( lexIdle( [ #"(", #")", #"[", #"]", #"\n" ], dummyStream ) )
      [ LEFTPAREN, RIGHTPAREN, LEFTSQ, RIGHTSQ, EOF ]
      ( eqOrderedList eqToken );
    (* single-quoted *)
    UnitTester.printResultPoly "lexIdle 10"
      ( lexIdle( 
        [ #"'", #"R", #"a", #"e", #" ", #"L", #"y", #"n", #"n", #"'", #"\n" ], 
          dummyStream ) )
      [ ATOM( "Rae Lynn" ), EOF ]
      ( eqOrderedList eqToken );
    (* double-quoted *)
    UnitTester.printResultPoly "lexIdle 11"
      ( lexIdle( 
        [ #"\"", #"R", #"a", #"e", #" ", #"L", #"y", #"n", #"n", #"\"", #"\n" ], 
          dummyStream ) )
      [ ATOM( "\"Rae Lynn\"" ), EOF ]
      ( eqOrderedList eqToken );
    (* arithmetic *)
    UnitTester.printResultPoly "lexIdle 12"
      ( lexIdle( [ #"+", #"-", #"*", #"/", #"\n" ], dummyStream ) )
      [ PLUS, MINUS, MULT, DIV, EOF ]
      ( eqOrderedList eqToken );
    (* pipe, dot, comma *)
    UnitTester.printResultPoly "lexIdle 13"
      ( lexIdle( [ #"|", #".", #",", #"\n" ], dummyStream ) )
      [ PIPE, DOT, COMMA, EOF ]
      ( eqOrderedList eqToken );
    (* comment *)
    UnitTester.printResultPoly "lexIdle 14"
      ( lexIdle( [ #"|", #"%", #"\n" ], dummyStream ) )
      [ PIPE, EOF ]
      ( eqOrderedList eqToken );
    (* proper test file *)
    UnitTester.printResultPoly "lexIdle 15"
      ( lexIdle( [ #"\n" ], 
        ( TextIO.openIn( "TestFiles/UnitTestFile1.pl" ) ) ) )
      [ ATOM( "ayla" ), LEFTPAREN, VARIABLE( "X" ), RIGHTPAREN, COLONMINUS, 
        VARIABLE( "Derpl" ), IS, INT( 45 ), PLUS, FLOAT( 2.2 ), COMMA, 
        ATOM( "reverse" ), LEFTSQ, VARIABLE( "X" ), PIPE, VARIABLE( "Derpl" ), 
        RIGHTSQ, DOT, COLONMINUS, ATOM( "ayla" ), LEFTPAREN, 
        ATOM( "foobar2202" ), RIGHTPAREN, DOT, EOF ]
      ( eqOrderedList eqToken )
    
    
  end
);
