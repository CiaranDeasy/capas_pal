(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the unit tests for functions defined in Utility.sml.
*******************************************************************************)

fun unitTestUtility() = (

    UnitTester.printResult "member 1" ( member 1 [1,2,3] ) true;
    UnitTester.printResult "member 2" ( member 3 [1,2,3] ) true;
    UnitTester.printResult "member 3" ( member 4 [1,2,3] ) false;
    UnitTester.printResult "member 4" ( member 4 [] ) false;
        
    UnitTester.printResult "memberPoly 1" ( 
      memberPoly ( Binding( Variable("1",0), Variable("2",0) ) ) 
        [ Binding( Variable("1",0), Variable("3",0) ), 
          Binding( Variable("1",0), Variable("2",0) ) ] 
        eqBinding
    ) true;
    
    UnitTester.printResult "memberPoly 2" ( 
      memberPoly ( Binding( Variable("1",0), Variable("2",0) ) ) 
        [ Binding( Variable("1",0), Variable("3",0) ), 
          Binding( Variable("1",0), Variable("4",0) ) ] 
        eqBinding
    ) false;
    
    UnitTester.printResult "memberPoly 3" ( 
      memberPoly ( Binding( Variable("1",0), Variable("2",0) ) ) [] eqBinding
    ) false;
    
    UnitTester.printResult "removeWithBlacklist 1" ( 
      removeWithBlacklist [2,3,4] [1,2,3] 
    ) [4];
    
    UnitTester.printResult "removeWithBlacklist 2"( 
      removeWithBlacklist [] [1,2,3] 
    ) [];

    UnitTester.printResult "removeWithBlacklist 3"( 
      removeWithBlacklist [2,3,4] [] 
    ) [2,3,4];

    UnitTester.printResult "zip 1" ( zip [1,2] [3,4] ) [(1,3),(2,4)];
    UnitTester.printResult "zip 2" ( zip [1] [2] ) [(1,2)];
    UnitTester.printResult "zip 3" ( zip [] [] ) [];

    UnitTester.printResult "first" ( first (1,2) ) 1;
    UnitTester.printResult "second" ( second (1,2) ) 2;

    UnitTester.printResult "andList 1" ( andList [true, false, true] ) false;
    UnitTester.printResult "andList 1" ( andList [true, true, true] ) true;
    UnitTester.printResult "andList 1" ( andList [] ) true;

    UnitTester.printResult "flip" ( flip (1,2) ) (2,1);
    
    UnitTester.printResult "eqOrderedList 1" (
      eqOrderedList ( eqTwoTuple op= op= ) ( 
        [ (1,2), (3,4) ], 
        [ (1,2), (3,4) ]
      )
    ) true;
    
    UnitTester.printResult "eqOrderedList 2" (
      eqOrderedList ( eqTwoTuple op= op= ) ( 
        [ (1,2), (3,4) ], 
        [ (1,2) ]
      )
    ) false;
    
    UnitTester.printResult "eqOrderedList 3" (
      eqOrderedList ( eqTwoTuple op= op= ) ( 
        [ (1,2), (3,4) ], 
        [ (1,2), (3,6) ]
      )
    ) false;
    
    UnitTester.printResult "eqOrderedList 4" (
      eqOrderedList ( eqTwoTuple op= op= ) ( [], [] )
    ) true;
    
    UnitTester.printResult "eqOrderedList 5" (
      eqOrderedList op= ( [], [ 1, 2 ] )
    ) false;
    
    UnitTester.printResult "eqOrderedList 6" (
      eqOrderedList op= ( [ 1, 2 ], [ 2, 1 ] )
    ) false;

	UnitTester.printResult "eqTwoTuple 1" (
	  eqTwoTuple op= eqBinding (
	    ( true, ( Binding( Variable("1",0), Variable("2",0) ) ) ), 
	    ( false, ( Binding( Variable("1",0), Variable("2",0) ) ) )
	  )
	) false;
    
	UnitTester.printResult "eqTwoTuple 2" (
	  eqTwoTuple op= eqBinding (
	    ( true, ( Binding( Variable("1",0), Variable("2",0) ) ) ), 
	    ( true, ( Binding( Variable("2",0), Variable("1",0) ) ) )
	  )
	) true;
    
	UnitTester.printResult "eqTwoTuple 3" (
	  eqTwoTuple op= eqBinding (
	    ( true, ( Binding( Variable("1",0), Variable("2",0) ) ) ), 
	    ( true, ( Binding( Variable("1",0), Variable("3",0) ) ) )
	  )
	) false;
    
    UnitTester.printResult "eqUnorderedList 1" (
      eqUnorderedList eqBinding ( 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ], 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ]
      )
    ) true;

    UnitTester.printResult "eqUnorderedList 2" (
      eqUnorderedList eqBinding ( 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ], 
        [ ( Binding( Variable("3",0), Variable("4",0) ) ), 
          ( Binding( Variable("1",0), Variable("2",0) ) ) ]
      )
    ) true;

    UnitTester.printResult "eqUnorderedList 3" (
      eqUnorderedList eqBinding ( 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ], 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("6",0) ) ) ]
      )
    ) false;

    UnitTester.printResult "eqUnorderedList 4" (
      eqUnorderedList op= ( [], [] )
    ) true;

    UnitTester.printResult "eqUnorderedList 5" (
      eqUnorderedList eqBinding ( [], [ 
        ( Binding( Variable("1",0), Variable("2",0) ) ) 
      ] )
    ) false;

    UnitTester.printResult "eqUnorderedList 6" (
      eqUnorderedList eqBinding ( 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ], 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ) ]
      )
    ) false;
    
    (* Simple match *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 1" (
      eqUnorderedListIgnoreDups eqBinding ( 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ], 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ]
      )
    ) true;
    (* Unordered match *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 2" (
      eqUnorderedListIgnoreDups eqBinding ( 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ], 
        [ ( Binding( Variable("3",0), Variable("4",0) ) ), 
          ( Binding( Variable("1",0), Variable("2",0) ) ) ]
      )
    ) true;
    (* In-order non-matching *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 3" (
      eqUnorderedListIgnoreDups eqBinding ( 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ], 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("6",0) ) ) ]
      )
    ) false;
    (* Empty lists *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 4" (
      eqUnorderedListIgnoreDups op= ( [], [] )
    ) true;
    (* One empty list *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 5" (
      eqUnorderedListIgnoreDups eqBinding ( [], [ 
        ( Binding( Variable("1",0), Variable("2",0) ) ) 
      ] )
    ) false;
    (* Asymmetric lists *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 6" (
      eqUnorderedListIgnoreDups eqBinding ( 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ), 
          ( Binding( Variable("3",0), Variable("4",0) ) ) ], 
        [ ( Binding( Variable("1",0), Variable("2",0) ) ) ]
      )
    ) false;
    (* Duplicates in one list *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 7" (
      eqUnorderedListIgnoreDups op= ( 
        [ 1, 1, 2, 3, 2 ], 
        [ 3, 2, 1 ]
      )
    ) true;
    (* Duplicates in both lists *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 8" (
      eqUnorderedListIgnoreDups op= ( 
        [ 1, 1, 2, 3, 2 ], 
        [ 3, 1, 2, 1, 3, 3 ]
      )
    ) true;
    (* Non-matching with duplicates *)
    UnitTester.printResult "eqUnorderedListIgnoreDups 9" (
      eqUnorderedListIgnoreDups op= ( 
        [ 1, 1, 2, 3, 2, 4 ], 
        [ 3, 1, 2, 1, 3, 3 ]
      )
    ) false
);
