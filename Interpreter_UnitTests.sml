fun unitTest() = 
  let val testsRun = ref 0 in 
  let val testsPassed = ref 0 in
  let fun printResultPoly testName actualResult expectedResult eqTest = (
	( testsRun := (!testsRun) + 1 );
    if( eqTest( actualResult, expectedResult ) )
	  then ( 
	    print "Test "; 
		print ( Int.toString (!testsRun) ); 
	    print ": \""; 
		print testName; 
		print "\" passed.\n";
        ( testsPassed := (!testsPassed) + 1 )
	  )
	else ( 
	  print "Test ";
	  print ( Int.toString (!testsRun) ); 
	  print ": \"";
	  print testName;
	  print "\" FAILED!!!!!\n"
	)
  ) in
  let fun printResult testName actualResult expectedResult = (
    printResultPoly testName actualResult expectedResult op=
  ) in
  let fun conclude() = ( 
    print ( Int.toString (!testsPassed) ); 
	print " out of ";
	print ( Int.toString (!testsRun) );
	print " tests passed.\n";
	print ( Int.toString ( (!testsRun) - (!testsPassed) ) );
	print " tests failed.\n"
  ) in
			
  ( printResult "member 1" ( member 1 [1,2,3] ) true;
    printResult "member 2" ( member 3 [1,2,3] ) true;
    printResult "member 3" ( member 4 [1,2,3] ) false;
    printResult "member 4" ( member 4 [] ) false;
    printResult "removeWithBlacklist 1" ( 
	  removeWithBlacklist [2,3,4] [1,2,3] 
	) [4];
    printResult "removeWithBlacklist 2"( removeWithBlacklist [] [1,2,3] ) [];
    printResult "removeWithBlacklist 3"( removeWithBlacklist [2,3,4] [] ) [2,3,4];
    printResult "zip 1" ( zip [1,2] [3,4] ) [(1,3),(2,4)];
    printResult "zip 2" ( zip [1] [2] ) [(1,2)];
    printResult "zip 3" ( zip [] [] ) [];
    printResult "first" ( first (1,2) ) 1;
    printResult "second" ( second (1,2) ) 2;
	printResult "andList 1" ( andList [true, false, true] ) false;
	printResult "andList 1" ( andList [true, true, true] ) true;
	printResult "andList 1" ( andList [] ) true;
	printResult "flip" ( flip (1,2) ) (2,1);
	printResult "flipBinding" ( 
	  flipBinding ( Binding( Variable("1"), Variable("2") ) ) 
	) (
	  Binding( Variable("2"), Variable("1") )
    );
	printResult "getTransitiveBinding 1" (
	  getTransitiveBinding ( Binding( Variable("1"), Variable("2") ) )
	  ( Binding( Variable("2"), Variable("3") ) ) 
	) ( true, ( Binding( Variable("1"), Variable("3") ) ) );
	printResult "getTransitiveBinding 2" (
	  getTransitiveBinding ( Binding( Variable("2"), Variable("1") ) )
	    ( Binding( Variable("2"), Variable("3") ) )
	) ( true, ( Binding( Variable("1"), Variable("3") ) ) );
	printResult "getTransitiveBinding 3" (
	  getTransitiveBinding ( Binding( Variable("1"), Variable("2") ) )
	    ( Binding( Variable("3"), Variable("2") ) ) 
	) ( true, ( Binding( Variable("1"), Variable("3") ) ) );
	printResult "getTransitiveBinding 4" (
	  getTransitiveBinding ( Binding( Variable("2"), Variable("1") ) )
	    ( Binding( Variable("3"), Variable("2") ) )
	) ( true, ( Binding( Variable("1"), Variable("3") ) ) );
	printResult "getTransitiveBinding 5" (
	  first( 
	    getTransitiveBinding ( Binding( Variable("1"), Variable("2") ) )
	      ( Binding( Variable("3"), Variable("4") ) )
      )
	) false;
	printResult "getTransitiveBinding 6" (
	  first( 
	    getTransitiveBinding ( Binding( Variable("1"), Variable("2") ) )
	      ( Binding( Variable("2"), Variable("1") ) )
      )
	) false;
	printResult "eqBinding 1" (
	  eqBinding( Binding( Variable("1"), Variable("2") ), 
	    Binding( Variable("1"), Variable("2") ) )
	) true;
	printResult "eqBinding 2" (
	  eqBinding( Binding( Variable("1"), Variable("2") ), 
	    Binding( Variable("2"), Variable("1") ) )
	) true;
	printResult "eqBinding 3" (
	  eqBinding( Binding( Variable("1"), Variable("2") ), 
	    Binding( Variable("1"), Variable("3") ) )
	) false;
	printResult "eqBinding 4" (
	  eqBinding( Binding( Variable("1"), Variable("2") ), 
	    Binding( Variable("3"), Variable("1") ) )
	) false;
	printResult "eqTupleBinding 1" (
	  eqTupleBinding(
	    ( true, ( Binding( Variable("1"), Variable("2") ) ) ), 
	    ( false, ( Binding( Variable("1"), Variable("2") ) ) )
	  )
	) false;
	printResult "eqTupleBinding 2" (
	  eqTupleBinding(
	    ( true, ( Binding( Variable("1"), Variable("2") ) ) ), 
	    ( true, ( Binding( Variable("2"), Variable("1") ) ) )
	  )
	) true;
	printResult "eqTupleBinding 3" (
	  eqTupleBinding( 
	    ( true, ( Binding( Variable("1"), Variable("2") ) ) ), 
	    ( true, ( Binding( Variable("1"), Variable("3") ) ) )
	  )
	) false;
	printResult "eqUnorderedBindingList 1" (
	  eqUnorderedBindingList( 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ], 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ]
	  )
	) true;
	printResult "eqUnorderedBindingList 2" (
	  eqUnorderedBindingList( 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ], 
	    [ ( Binding( Variable("3"), Variable("4") ) ), ( Binding( Variable("1"), Variable("2") ) ) ]
	  )
	) true;
	printResult "eqUnorderedBindingList 3" (
	  eqUnorderedBindingList( 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ], 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("6") ) ) ]
	  )
	) false;
	printResult "eqUnorderedBindingList 4" (
	  eqUnorderedBindingList( [], [] )
	) true;
	printResult "eqUnorderedBindingList 5" (
	  eqUnorderedBindingList( [], [ ( Binding( Variable("1"), Variable("2") ) ) ] )
	) false;
	printResult "eqUnorderedBindingList 6" (
	  eqUnorderedBindingList( 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ], 
	    [ ( Binding( Variable("1"), Variable("2") ) ) ]
	  )
	) false;
	printResultPoly "getAllTransitiveBindings 1" (
	  getAllTransitiveBindings ( Binding( Variable("1"), Variable("2") ) )
	      [ ( Binding( Variable("2"), Variable("3") ) ),
		    ( Binding( Variable("3"), Variable("4") ) ),
		    ( Binding( Variable("4"), Variable("2") ) ) ]
	) [ ( Binding( Variable("1"), Variable("3") ) ), 
	    ( Binding( Variable("1"), Variable("4") ) ) 
	  ] eqUnorderedBindingList;
	printResultPoly "getAllTransitiveBindings 2" (
	  getAllTransitiveBindings ( Binding( Variable("1"), Variable("2") ) )
	      []
	) [] eqUnorderedBindingList;
	printResultPoly "getAllTransitiveBindings 3" (
	  getAllTransitiveBindings ( Binding( Variable("1"), Variable("5") ) )
	      [ ( Binding( Variable("2"), Variable("3") ) ),
		    ( Binding( Variable("3"), Variable("4") ) ),
		    ( Binding( Variable("4"), Variable("2") ) ) ]
	) [] eqUnorderedBindingList;
	
	  
	conclude()
  )
	
  end end end end end;
  