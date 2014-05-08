(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the unit tests for functions defined in 
Interpreter.sml.
*******************************************************************************)

fun unitTestParser() = (

    (* parseFactor ---------------------------------------------------------- *)
    (* Int *)
    UnitTester.printResultPoly "parseFactor 1" 
      ( parseFactor( [ INT( 1 ), EOF ] ) )
      ( IntTerm( 1 ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Float *)
    UnitTester.printResultPoly "parseFactor 2" 
      ( parseFactor( [ FLOAT( 1.11 ), EOF ] ) )
      ( FloatTerm( 1.11 ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Simple bracketed expression *)
    UnitTester.printResultPoly "parseFactor 3" 
      ( parseFactor( [ LEFTPAREN, INT( 2 ), RIGHTPAREN, EOF ] ) )
      ( IntTerm( 2 ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Complex bracketed expression *)
    UnitTester.printResultPoly "parseFactor 4" 
      ( parseFactor( [ LEFTPAREN, INT( 2 ), PLUS, LEFTPAREN, FLOAT(3.14), DIV, 
          INT(3), RIGHTPAREN, RIGHTPAREN, EOF ] ) )
      ( Term( 
          Functor("+"), 
          [
            IntTerm( 2 ),
            Term( 
              Functor("/"), 
              [
                FloatTerm(3.14),
                IntTerm(3)
              ]
            )
          ]
        ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
      
    (* parseMoreFactors ----------------------------------------------------- *)
    (* MULT *)
    UnitTester.printResultPoly "parseMoreFactors 1" 
      ( parseMoreFactors( [ MULT, INT( 2 ), EOF ], IntTerm( 1 ) ) )
      ( Term( Functor("*"), [ IntTerm( 1 ), IntTerm( 2 ) ] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* DIV *)
    UnitTester.printResultPoly "parseMoreFactors 2" 
      ( parseMoreFactors( [ DIV, FLOAT( 2.2 ), EOF ], IntTerm( 1 ) ) )
      ( Term( Functor("/"), [ IntTerm( 1 ), FloatTerm( 2.2 ) ] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* MOD *)
    UnitTester.printResultPoly "parseMoreFactors 3" 
      ( parseMoreFactors( [ MOD, FLOAT( 2.2 ), EOF ], FloatTerm( 1.1 ) ) )
      ( Term( Functor("%"), [ FloatTerm( 1.1 ), FloatTerm( 2.2 ) ] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* No more factors *)
    UnitTester.printResultPoly "parseMoreFactors 4" 
      ( parseMoreFactors( [ EOF ], FloatTerm( 1.1 ) ) )
      ( FloatTerm( 1.1 ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    
    (* parseArithTerm ------------------------------------------------------- *)
    (* Single factor *)
    UnitTester.printResultPoly "parseArithTerm 1" 
      ( parseArithTerm( [ INT( 1 ), EOF ] ) )
      ( IntTerm( 1 ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Two factors *)
    UnitTester.printResultPoly "parseArithTerm 2" 
      ( parseArithTerm( [ INT( 1 ), MULT, FLOAT( 2.0 ), EOF ] ) )
      ( Term( Functor("*"), [ IntTerm( 1 ), FloatTerm( 2.0 ) ] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Trick "PLUS" token should be ignored. *)
    UnitTester.printResultPoly "parseArithTerm 3" 
      ( parseArithTerm( [ INT( 1 ), PLUS, FLOAT( 2.0 ), EOF ] ) )
      ( IntTerm( 1 ), [ PLUS, FLOAT( 2.0 ), EOF ] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
      
    (* parseMoreArithTerms -------------------------------------------------- *)
    (* PLUS *)
    UnitTester.printResultPoly "parseMoreArithTerms 1" 
      ( parseMoreArithTerms( [ PLUS, FLOAT( 2.0 ), EOF ], IntTerm( 1 ) ) )
      ( Term( Functor("+"), [ IntTerm( 1 ), FloatTerm( 2.0 ) ] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* MINUS *)
    UnitTester.printResultPoly "parseMoreArithTerms 2" 
      ( parseMoreArithTerms( [ MINUS, FLOAT( 2.0 ), EOF ], IntTerm( 1 ) ) )
      ( Term( Functor("-"), [ IntTerm( 1 ), FloatTerm( 2.0 ) ] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* No more terms *)
    UnitTester.printResultPoly "parseMoreArithTerms 3" 
      ( parseMoreArithTerms( [ EOF ], IntTerm( 1 ) ) )
      ( IntTerm( 1 ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    
    (* parseArith ----------------------------------------------------------- *)
    (* Single term *)
    UnitTester.printResultPoly "parseArith 1" 
      ( parseArith( [ INT( 1 ), EOF ] ) )
      ( IntTerm( 1 ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Two terms *)
    UnitTester.printResultPoly "parseArith 2" 
      ( parseArith( [ INT( 1 ), PLUS, FLOAT( 2.2 ), EOF ] ) )
      ( Term( Functor("+"), [ IntTerm( 1 ), FloatTerm( 2.2 ) ] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Term with factors. *)
    UnitTester.printResultPoly "parseArith 3" 
      ( parseArith( [ INT( 1 ), MULT, FLOAT( 2.0 ), PLUS, INT( 3 ), EOF ] ) )
      ( Term(
          Functor("+"),
          [
            Term( Functor("*"), [ IntTerm( 1 ), FloatTerm( 2.0 ) ] ),
            IntTerm( 3 )
          ]
        ), 
        [ EOF ] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Same with reversed operators. *)
    UnitTester.printResultPoly "parseArith 4" 
      ( parseArith( [ INT( 1 ), PLUS, FLOAT( 2.0 ), MULT, INT( 3 ), EOF ] ) )
      ( Term(
          Functor("+"),
          [
            IntTerm( 1 ),
            Term( Functor("*"), [ FloatTerm( 2.0 ), IntTerm( 3 ) ] )
          ]
        ), 
        [ EOF ] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
      
    (* parseTail ------------------------------------------------------------ *)
    (* Variable *)
    UnitTester.printResultPoly "parseTail 1" 
      ( parseTail( [ VARIABLE( "Derpl" ), RIGHTSQ, EOF ] ) )
      ( Variable( "Derpl", 0 ), [ RIGHTSQ, EOF ] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* List *)
    UnitTester.printResultPoly "parseTail 2" 
      ( parseTail( [ LEFTSQ, ATOM( "Ayla" ), RIGHTSQ, RIGHTSQ, EOF ] ) )
      ( Term( 
          Functor("."), 
          [ 
            Term( Functor("Ayla"), [] ), Term( Functor("[]"), [] ) 
          ] 
        ), [ RIGHTSQ, EOF ] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* Empty list *)
    UnitTester.printResultPoly "parseTail 3" 
      ( parseTail( [ LEFTSQ, RIGHTSQ, RIGHTSQ, EOF ] ) )
      ( Term( Functor("[]"), [] ), [ RIGHTSQ, EOF ] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    
    (* parseIsTerm ---------------------------------------------------------- *)
    UnitTester.printResultPoly "parseIsTerm 1"
      ( parseIsTerm( 
          [ IS, ATOM( "derpl" ), EOF ], Term( Functor("ayla"), [] ) ) )
      ( Term( 
          Functor("is"), 
          [ 
            Term( Functor("ayla"), [] ), 
            Term( Functor("derpl"), [] ) 
          ] 
        ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    
    (* parseMoreList -------------------------------------------------------- *)
    (* [ A, B, C ] *)
    UnitTester.printResultPoly "parseMoreList 1"
      ( parseMoreList( [ RIGHTSQ, EOF ] ) )
      ( Term( Functor("[]"), [] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    (* [ H | T ] *)
    UnitTester.printResultPoly "parseMoreList 2"
      ( parseMoreList( [ PIPE, VARIABLE("T"), RIGHTSQ, EOF ] ) )
      ( Variable( "T", 0 ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) );
    
    (* parseTerm ------------------------------------------------------------ *)
    (* Term without args *)
    UnitTester.printResultPoly "parseTerm 1"
      ( parseTerm( [ ATOM("derpl"), EOF ] ) )
      ( Term( Functor("derpl"), [] ), [EOF] )
      ( eqTwoTuple eqTerm ( eqOrderedList eqToken ) )
);