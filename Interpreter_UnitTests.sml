fun unitTest() = 
  let val testNo = ref 1 in 
  let fun printResultPoly actualResult expectedResult eqTest = (
    if( eqTest( actualResult, expectedResult ) )
	  then ( print "Test "; print ( Int.toString (!testNo) ); 
	    print " passed.\n" )
	else ( print "Test "; print ( Int.toString (!testNo) ); 
	    print " FAILED!!!!!\n" ); ( testNo := (!testNo) + 1 ) 
  ) in
  let fun printResult actualResult expectedResult = (
    printResultPoly actualResult expectedResult op=
  ) in
			
  ( printResult ( member 1 [1,2,3] ) true;
    printResult ( member 3 [1,2,3] ) true;
    printResult ( member 4 [1,2,3] ) false;
    printResult ( member 4 [] ) false;
    printResult ( removeWithBlacklist [2,3,4] [1,2,3] ) [4];
    printResult ( removeWithBlacklist [] [1,2,3] ) [];
    printResult ( removeWithBlacklist [2,3,4] [] ) [2,3,4];
    printResult ( zip [1,2] [3,4] ) [(1,3),(2,4)];
    printResult ( zip [1] [2] ) [(1,2)];
    printResult ( zip [] [] ) [];
		  
    printResult ( first (1,2) ) 1;
    printResult ( second (1,2) ) 2;
	printResult ( andList [true, false, true] ) false;
	printResult ( andList [true, true, true] ) true;
	printResult ( andList [] ) true;
	printResult ( flip (1,2) ) (2,1);
	printResult ( 
	  flipBinding ( Binding( Variable("1"), Variable("2") ) ) 
	) (
	  Binding( Variable("2"), Variable("1") )
    );
	printResult (
	  getTransitiveBinding ( Binding( Variable("1"), Variable("2") ) )
	  ( Binding( Variable("2"), Variable("3") ) ) 
	) ( true, ( Binding( Variable("1"), Variable("3") ) ) );
	printResult (
	  getTransitiveBinding ( Binding( Variable("2"), Variable("1") ) )
	    ( Binding( Variable("2"), Variable("3") ) )
	) ( true, ( Binding( Variable("1"), Variable("3") ) ) );
	printResult (
	  getTransitiveBinding ( Binding( Variable("1"), Variable("2") ) )
	    ( Binding( Variable("3"), Variable("2") ) ) 
	) ( true, ( Binding( Variable("1"), Variable("3") ) ) );
	  
	printResult (
	  getTransitiveBinding ( Binding( Variable("2"), Variable("1") ) )
	    ( Binding( Variable("3"), Variable("2") ) )
	) ( true, ( Binding( Variable("1"), Variable("3") ) ) );
	printResult (
	  first( 
	    getTransitiveBinding ( Binding( Variable("1"), Variable("2") ) )
	      ( Binding( Variable("3"), Variable("4") ) )
      )
	) false;
	printResult (
	  eqBinding( Binding( Variable("1"), Variable("2") ), 
	    Binding( Variable("1"), Variable("2") ) )
	) true;
	printResult (
	  eqBinding( Binding( Variable("1"), Variable("2") ), 
	    Binding( Variable("2"), Variable("1") ) )
	) true;
	printResult (
	  eqBinding( Binding( Variable("1"), Variable("2") ), 
	    Binding( Variable("1"), Variable("3") ) )
	) false;
	printResult (
	  eqBinding( Binding( Variable("1"), Variable("2") ), 
	    Binding( Variable("3"), Variable("1") ) )
	) false;
	printResult (
	  eqTupleBinding(
	    ( true, ( Binding( Variable("1"), Variable("2") ) ) ), 
	    ( false, ( Binding( Variable("1"), Variable("2") ) ) )
	  )
	) false;
	printResult (
	  eqTupleBinding(
	    ( true, ( Binding( Variable("1"), Variable("2") ) ) ), 
	    ( true, ( Binding( Variable("2"), Variable("1") ) ) )
	  )
	) true;
	printResult (
	  eqTupleBinding( 
	    ( true, ( Binding( Variable("1"), Variable("2") ) ) ), 
	    ( true, ( Binding( Variable("1"), Variable("3") ) ) )
	  )
	) false;
	printResult (
	  eqBindingList( 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ], 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ]
	  )
	) true;
	printResult (
	  eqBindingList( 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ], 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("6") ) ) ]
	  )
	) false;
	printResult (
	  eqBindingList( [], [] )
	) true;
	printResult (
	  eqBindingList( [], [ ( Binding( Variable("1"), Variable("2") ) ) ] )
	) false;
	printResult (
	  eqBindingList( 
	    [ ( Binding( Variable("1"), Variable("2") ) ), ( Binding( Variable("3"), Variable("4") ) ) ], 
	    [ ( Binding( Variable("1"), Variable("2") ) ) ]
	  )
	) false
	
    (*printResultPoly (
	  getAllTransitiveBindings ( Binding( Variable("1"), Variable("2") ) )
	      [ ( Binding( Variable("2"), Variable("3") ) ),
		    ( Binding( Variable("3"), Variable("4") ) ),
		    ( Binding( Variable("4"), Variable("2") ) ) ]
	) [ ( Binding( Variable("1"), Variable("3") ) ), 
	    ( Binding( Variable("1"), Variable("4") ) ) 
	  ] eqBinding*)
  )
	
  end end end;
  