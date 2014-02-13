(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the unit tests for functions defined in Datatypes.sml.
*******************************************************************************)

fun unitTestDatatypes() = (
	UnitTester.printResultPoly "flipBinding" ( 
	  flipBinding ( Binding( Variable("1",0), Variable("2",0) ) ) 
	) (
	  Binding( Variable("2",0), Variable("1",0) )
    )
    eqBinding;
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
	) false

)
