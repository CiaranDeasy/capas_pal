(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the unit tests for functions defined in Datatypes.sml.
*******************************************************************************)

fun unitTestDatatypes() = (
    
    (* (T, T), no args, matching *)
    UnitTester.printResult "eqTerm 1" (
      eqTerm( 
        Term( Functor("1"), [] ),
        Term( Functor("1"), [] )
      )
    ) true;
    
    (* (T, T), no args, not matching *)
    UnitTester.printResult "eqTerm 2" (
      eqTerm( 
        Term( Functor("1"), [] ),
        Term( Functor("2"), [] )
      )
    ) false;
    
    (* (T, T), single arg, matching *)
    UnitTester.printResult "eqTerm 3" (
      eqTerm( 
        Term( Functor("1"), [ Term( Functor("2"), [] ) ] ),
        Term( Functor("1"), [ Term( Functor("2"), [] ) ] )
      )
    ) true;
    
    (* (T, T), single arg, not matching *)
    UnitTester.printResult "eqTerm 4" (
      eqTerm( 
        Term( Functor("1"), [ Term( Functor("2"), [] ) ] ),
        Term( Functor("1"), [ Term( Functor("3"), [] ) ] )
      )
    ) false;
    
    (* (T, T), differing number of args *)
    UnitTester.printResult "eqTerm 5" (
      eqTerm( 
        Term( Functor("1"), [ Term( Functor("2"), [] ) ] ),
        Term( 
          Functor("1"), 
          [ 
            Term( Functor("2"), [] ), 
            Term( Functor("3"), [] )
          ]
        )
      )
    ) false;
    
    (* (T, T), multiple args, matching *)
    UnitTester.printResult "eqTerm 6" (
      eqTerm( 
        Term( 
          Functor("1"),
          [ 
            Term( Functor("2"), [] ),
            Term( Functor("3"), [] )
          ] 
        ),
        Term( 
          Functor("1"), 
          [ 
            Term( Functor("2"), [] ), 
            Term( Functor("3"), [] )
          ]
        )
      )
    ) true;
    
    (* (V, V), matching *)
    UnitTester.printResult "eqTerm 7" (
      eqTerm( Variable("1",0), Variable("1",0) )
    ) true;
    
    (* (V, V), different name *)
    UnitTester.printResult "eqTerm 8" (
      eqTerm( Variable("1",0), Variable("2",0) )
    ) false;
    
    (* (V, V), different scope *)
    UnitTester.printResult "eqTerm 9" (
      eqTerm( Variable("1",1), Variable("1",0) )
    ) false;
    
    (* (I, I), matching *)
    UnitTester.printResult "eqTerm 10" (
      eqTerm( IntTerm(1), IntTerm(1) )
    ) true;
    
    (* (I, I), not matching *)
    UnitTester.printResult "eqTerm 11" (
      eqTerm( IntTerm(1), IntTerm(2) )
    ) false;
    
    (* (F, F), matching *)
    UnitTester.printResult "eqTerm 12" (
      eqTerm( FloatTerm(1.0), FloatTerm(1.0) )
    ) true;
    
    (* (F, F), not matching *)
    UnitTester.printResult "eqTerm 13" (
      eqTerm( FloatTerm(1.0), FloatTerm(1.1) )
    ) false;
    
    (* (T, V) *)
    UnitTester.printResult "eqTerm 14" (
      eqTerm( Term( Functor("1"), [] ), Variable("1",0) )
    ) false;
    
    (* (T, I) *)
    UnitTester.printResult "eqTerm 15" (
      eqTerm( Term( Functor("1"), [] ), IntTerm(1) )
    ) false;
    
    (* (T, F) *)
    UnitTester.printResult "eqTerm 16" (
      eqTerm( Term( Functor("1"), [] ), FloatTerm(1.0) )
    ) false;
    
    (* (V, I) *)
    UnitTester.printResult "eqTerm 17" (
      eqTerm( Variable("1",0), IntTerm(1) )
    ) false;
    
    (* (V, F) *)
    UnitTester.printResult "eqTerm 18" (
      eqTerm( Variable("1",0), FloatTerm(1.0) )
    ) false;
    
    (* (I, F) *)
    UnitTester.printResult "eqTerm 19" (
      eqTerm( IntTerm(1), FloatTerm(1.0) )
    ) false;
    
    (* Awkward float number *)
    UnitTester.printResult "eqTerm 20" (
      eqTerm( FloatTerm(12.45), FloatTerm(12.45) )
    ) true;
    
    (* ---------------------------------------------------------------------- *)
    
    UnitTester.printResult "eqBinding 1" (
      eqBinding( Binding( Variable("1",0), Variable("2",0) ), 
        Binding( Variable("1",0), Variable("2",0) ) )
    ) true;
    UnitTester.printResult "eqBinding 2" (
      eqBinding( Binding( Variable("1",0), Variable("2",0) ), 
        Binding( Variable("2",0), Variable("1",0) ) )
    ) true;
    UnitTester.printResult "eqBinding 3" (
      eqBinding( Binding( Variable("1",0), Variable("2",0) ), 
        Binding( Variable("1",0), Variable("3",0) ) )
    ) false;
    UnitTester.printResult "eqBinding 4" (
      eqBinding( Binding( Variable("1",0), Variable("2",0) ), 
        Binding( Variable("3",0), Variable("1",0) ) )
    ) false;
    
    UnitTester.printResult "eqUnifier 1" (
      eqUnifier( Unifier(
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ]), Unifier(
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ])
      )
    ) true;
    
    UnitTester.printResult "eqUnifier 2" (
      eqUnifier( Unifier(
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ]), Unifier(
        [ ( Binding( Variable("3",0), Variable("4",0) ) ), 
          ( Binding( Variable("1",0), Variable("2",0) ) ) ])
      )
    ) true;
    
    UnitTester.printResult "eqUnifier 3" (
      eqUnifier( Unifier(
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ]), Unifier(
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("6",0) ) ) ])
      )
    ) false;
    
    UnitTester.printResult "eqUnifier 4" (
      eqUnifier( Unifier([]), Unifier([]) )
    ) true;
    
    UnitTester.printResult "eqUnifier 5" (
      eqUnifier( Unifier([]), Unifier([ 
        ( Binding( Variable("1",0), Variable("2",0) ) ) 
      ]) )
    ) false;
    
    UnitTester.printResult "eqUnifier 6" (
      eqUnifier( Unifier(
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
        ( Binding( Variable("3",0), Variable("4",0) ) ) ]), Unifier(
        [ ( Binding( Variable("1",0), Variable("2",0) ) ) ])
      )
    ) false;
    
    (* Simple case *)
    UnitTester.printResult "eqClause 1" (
      eqClause( 
        Clause( Term( Functor("1"), [] ), [] ),
        Clause( Term( Functor("1"), [] ), [] )
      )
    ) true;
    
    (* Simple false case *)
    UnitTester.printResult "eqClause 2" (
      eqClause( 
        Clause( Term( Functor("1"), [] ), [] ),
        Clause( Term( Functor("2"), [] ), [] )
      )
    ) false;
    
    (* With body true *)
    UnitTester.printResult "eqClause 3" (
      eqClause( 
        Clause( Term( Functor("1"), [ Variable("3",0) ] ), [] ),
        Clause( Term( Functor("1"), [ Variable("3",0) ] ), [] )
      )
    ) true;
    
    (* With body false *)
    UnitTester.printResult "eqClause 4" (
      eqClause( 
        Clause( Term( Functor("1"), [ Variable("3",0) ] ), [] ),
        Clause( Term( Functor("1"), [ Variable("4",0) ] ), [] )
      )
    ) false;
    
    (* Different body length *)
    UnitTester.printResult "eqClause 5" (
      eqClause( 
        Clause( Term( Functor("1"), [ Variable("3",0) ] ), [] ),
        Clause( Term( Functor("1"), [ Variable("3",0), Variable("5",0) ] ), [] )
      )
    ) false;
    
    (* Different term type *)
    UnitTester.printResult "eqClause 6" (
      eqClause( 
        Clause( Term( Functor("1"), [ Variable("3",0) ] ), [] ),
        Clause( Term( Functor("1"), [ Term( Functor("3"), [] ) ] ), [] )
      )
    ) false;

    UnitTester.printResultPoly "flipBinding" ( 
      flipBinding ( Binding( Variable("1",0), Variable("2",0) ) ) 
    ) (
      Binding( Variable("2",0), Variable("1",0) )
    )
    eqBinding

);
