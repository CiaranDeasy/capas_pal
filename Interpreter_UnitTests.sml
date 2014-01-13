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
	printResult "memberPoly 1" ( 
	  memberPoly ( Binding( Variable("1"), Variable("2") ) ) 
	    [ Binding( Variable("1"), Variable("3") ), 
		  Binding( Variable("1"), Variable("2") ) ] 
		eqBinding
	) true;
	printResult "memberPoly 2" ( 
	  memberPoly ( Binding( Variable("1"), Variable("2") ) ) 
	    [ Binding( Variable("1"), Variable("3") ), 
		  Binding( Variable("1"), Variable("4") ) ] 
		eqBinding
	) false;
	printResult "memberPoly 3" ( 
	  memberPoly ( Binding( Variable("1"), Variable("2") ) ) [] eqBinding
	) false;
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
	printResult "eqUnifier 1" (
	  eqUnifier( Unifier(
	    [ ( Binding( Variable("1"), Variable("2") ) ), 
          ( Binding( Variable("3"), Variable("4") ) ) ]), Unifier(
	    [ ( Binding( Variable("1"), Variable("2") ) ), 
          ( Binding( Variable("3"), Variable("4") ) ) ])
	  )
	) true;
	printResult "eqUnifier 2" (
	  eqUnifier( Unifier(
	    [ ( Binding( Variable("1"), Variable("2") ) ), 
          ( Binding( Variable("3"), Variable("4") ) ) ]), Unifier(
	    [ ( Binding( Variable("3"), Variable("4") ) ), 
          ( Binding( Variable("1"), Variable("2") ) ) ])
	  )
	) true;
	printResult "eqUnifier 3" (
	  eqUnifier( Unifier(
	    [ ( Binding( Variable("1"), Variable("2") ) ), 
          ( Binding( Variable("3"), Variable("4") ) ) ]), Unifier(
	    [ ( Binding( Variable("1"), Variable("2") ) ), 
          ( Binding( Variable("3"), Variable("6") ) ) ])
	  )
	) false;
	printResult "eqUnifier 4" (
	  eqUnifier( Unifier([]), Unifier([]) )
	) true;
	printResult "eqUnifier 5" (
	  eqUnifier( Unifier([]), Unifier([ ( Binding( Variable("1"), Variable("2") ) ) ]) )
	) false;
	printResult "eqUnifier 6" (
	  eqUnifier( Unifier(
	    [ ( Binding( Variable("1"), Variable("2") ) ), 
        ( Binding( Variable("3"), Variable("4") ) ) ]), Unifier(
	    [ ( Binding( Variable("1"), Variable("2") ) ) ])
	  )
	) false;
	printResult "eqTupleUnifier 1" (
	  eqTupleUnifier( 
        ( true, Unifier(
	      [ ( Binding( Variable("1"), Variable("2") ) ), 
            ( Binding( Variable("3"), Variable("4") ) ) ]) ), 
        ( false, Unifier(
	      [ ( Binding( Variable("1"), Variable("2") ) ), 
            ( Binding( Variable("3"), Variable("4") ) ) ]) )
	  )
	) false;
	printResult "eqTupleUnifier 2" (
	  eqTupleUnifier( 
        ( false, Unifier(
	      [ ( Binding( Variable("1"), Variable("2") ) ), 
            ( Binding( Variable("3"), Variable("4") ) ) ]) ), 
        ( false, Unifier(
	      [ ( Binding( Variable("3"), Variable("4") ) ), 
            ( Binding( Variable("1"), Variable("2") ) ) ]) )
	  )
	) true;
	printResult "eqTupleUnifier 3" (
	  eqTupleUnifier( 
        ( true, Unifier(
	      [ ( Binding( Variable("1"), Variable("2") ) ), 
            ( Binding( Variable("3"), Variable("4") ) ) ]) ), 
        ( true, Unifier(
	      [ ( Binding( Variable("1"), Variable("2") ) ), 
            ( Binding( Variable("3"), Variable("6") ) ) ]) )
	  )
	) false;
	printResult "eqTupleUnifier 4" (
	  eqTupleUnifier( ( true, Unifier([]) ), ( true, Unifier([]) ) )
	) true;
	printResult "eqTupleUnifier 5" (
	  eqTupleUnifier( ( true, Unifier([]) ), 
        ( true, Unifier([ ( Binding( Variable("1"), Variable("2") ) ) ]) ) )
	) false;
	printResult "eqTupleUnifier 6" (
	  eqTupleUnifier( 
        ( true, Unifier(
	      [ ( Binding( Variable("1"), Variable("2") ) ), 
            ( Binding( Variable("3"), Variable("4") ) ) ]) ), 
        ( true, Unifier(
	      [ ( Binding( Variable("1"), Variable("2") ) ) ]) )
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
	
	printResultPoly "getTransitiveClosure 1" (
	  getTransitiveClosure [ ( Binding( Variable("1"), Variable("2") ) ),
	                         ( Binding( Variable("3"), Variable("2") ) ),
	                         ( Binding( Variable("2"), Variable("4") ) ),
	                         ( Binding( Variable("5"), Variable("7") ) ),
	                         ( Binding( Variable("7"), Variable("6") ) ) ]
	) [ ( Binding( Variable("1"), Variable("2") ) ),
	    ( Binding( Variable("1"), Variable("3") ) ),
	    ( Binding( Variable("1"), Variable("4") ) ),
	    ( Binding( Variable("2"), Variable("3") ) ),
	    ( Binding( Variable("2"), Variable("4") ) ),
	    ( Binding( Variable("3"), Variable("4") ) ),
	    ( Binding( Variable("5"), Variable("6") ) ),
	    ( Binding( Variable("5"), Variable("7") ) ),
	    ( Binding( Variable("6"), Variable("7") ) ) ]
		  eqUnorderedBindingList;
		  
    printResultPoly "getTransitiveClosure 2" (
	  getTransitiveClosure [ ( Binding( Variable("1"), Variable("2") ) ),
	                         ( Binding( Variable("1"), Variable("3") ) ),
	                         ( Binding( Variable("1"), Variable("4") ) ),
	                         ( Binding( Variable("2"), Variable("3") ) ),
	                         ( Binding( Variable("2"), Variable("4") ) ),
	                         ( Binding( Variable("3"), Variable("4") ) ),
	                         ( Binding( Variable("5"), Variable("6") ) ),
	                         ( Binding( Variable("5"), Variable("7") ) ),
	                         ( Binding( Variable("6"), Variable("7") ) ) ]
	) [ ( Binding( Variable("1"), Variable("2") ) ),
	    ( Binding( Variable("1"), Variable("3") ) ),
	    ( Binding( Variable("1"), Variable("4") ) ),
	    ( Binding( Variable("2"), Variable("3") ) ),
	    ( Binding( Variable("2"), Variable("4") ) ),
	    ( Binding( Variable("3"), Variable("4") ) ),
	    ( Binding( Variable("5"), Variable("6") ) ),
	    ( Binding( Variable("5"), Variable("7") ) ),
	    ( Binding( Variable("6"), Variable("7") ) ) ]
		  eqUnorderedBindingList;
		  
    printResultPoly "getTransitiveClosure 3" (
	  getTransitiveClosure []
	) [] eqUnorderedBindingList;
	
    printResultPoly "combineUnifiers 1" ( combineUnifiers [] ) [] eqUnorderedBindingList;
    
    printResultPoly "combineUnifiers 2" ( 
      combineUnifiers [ Unifier([ Binding( Variable("1"), Variable("2") ) ]) ] 
    ) [ Binding( Variable("1"), Variable("2") ) ] eqUnorderedBindingList;
    
    printResultPoly "combineUnifiers 3" ( 
      combineUnifiers [ Unifier([ Binding( Variable("1"), Variable("2") ), 
                                  Binding( Variable("3"), Variable("4") ) ]),
                        Unifier([ Binding( Variable("2"), Variable("3") ) ]) ]
    ) [ Binding( Variable("1"), Variable("2") ), 
        Binding( Variable("3"), Variable("4") ),
        Binding( Variable("2"), Variable("3") ) ]
      eqUnorderedBindingList;
    
    printResultPoly "unify 1" (
      unify (Unifier([])) ( Binding( Variable("1"), Variable("2") ) )
    ) ( true, Unifier([ Binding( Variable("1"), Variable("2") ) ]) ) 
      eqTupleUnifier;
    
    printResultPoly "unify 2" (
      unify (Unifier([])) ( Binding( Term( Functor("1"), [] ), Variable("2") ) )
    ) ( true, Unifier([ Binding( Term( Functor("1"), [] ), Variable("2") ) ]) )
      eqTupleUnifier;
    
    printResultPoly "unify 3" (
      unify (Unifier([])) ( Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ), 
        Variable("2") 
      ) )
    ) ( true, Unifier([ Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ), 
        Variable("2") 
      ) ]) )
      eqTupleUnifier;
    
    printResultPoly "unify 4" (
      unify (Unifier([])) ( Binding( 
        Variable("2"), 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] )
      ) )
    ) ( true, Unifier([ Binding( 
        Variable("2"), 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ) 
      ) ]) )
      eqTupleUnifier;
    
    printResultPoly "unify 5" (
      unify (Unifier([])) ( Binding( 
        Term( Functor("1"), [] ),
        Term( Functor("1"), [] )
      ) )
    ) ( true, Unifier([]) )
      eqTupleUnifier;
    
    printResult "unify 6" (
      first( unify (Unifier([])) ( Binding( 
        Term( Functor("1"), [] ),
        Term( Functor("2"), [] )
      ) ) )
    ) false;
    
    printResultPoly "unify 7" (
      unify (Unifier([])) ( Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ),
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] )
      ) )
    ) ( true, Unifier([]) )
      eqTupleUnifier;
    
    printResult "unify 8" (
      first( unify (Unifier([])) ( Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ),
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("4"), [] ) ] )
      ) ) )
    ) false;
    
    printResultPoly "unify 9" (
      unify (Unifier([])) ( Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), 
            [ Variable("5") ] ), 
          Term( Functor("3"), [] ) ] ),
        Term( Functor("1"), 
          [ Term( Functor("2"), 
            [ Term( Functor("6"), [] ) ] ), 
          Variable("4") ] )
      ) )
    ) ( true, Unifier([ Binding( 
        Variable("4"), 
        Term( Functor("3"), [] ) 
      ), Binding( 
        Term( Functor("6"), [] ), 
        Variable("5") 
      ) ]) )
      eqTupleUnifier;
    
    printResultPoly "unify 10" 
    (* Test *)
    (
      unify (
        Unifier([ 
          Binding( 
            Term( Functor("6"), [] ), 
            Variable("5") 
          ) 
        ])
      ) 
      ( 
        Binding( 
          Term( 
            Functor("1"), 
            [ 
              Term( 
                Functor("2"), 
                [ Variable("5") ] 
              ), 
              Term( Functor("3"), [] ) 
            ] 
          ),
          Term( 
            Functor("1"), 
            [ 
              Term( 
                Functor("2"), 
                [ Term( Functor("6"), [] ) ] 
              ), 
            Variable("4") ] 
          )
        ) 
      )
    ) 
    (* Result *)
    ( 
      true, 
      Unifier([ 
        Binding( 
          Variable("4"), 
          Term( Functor("3"), [] ) 
        ), 
        Binding( 
          Term( Functor("6"), [] ), 
          Variable("5") 
        ) 
      ]) 
    )
    (* Equality test *)
    eqTupleUnifier;
    
    (* ------------------------------- *)
    
    printResultPoly "unify 11" 
    (* Test *)
    (
      unify (
        Unifier([ 
          Binding( 
            Term( Functor("8"), [] ), 
            Variable("9") 
          ) 
        ])
      ) 
      ( 
        Binding( 
          Term( 
            Functor("1"), 
            [ 
              Term( 
                Functor("2"), 
                [ Variable("5") ] 
              ), 
              Term( Functor("3"), [] ) 
            ] 
          ),
          Term( 
            Functor("1"), 
            [ 
              Term( 
                Functor("2"), 
                [ Term( Functor("6"), [] ) ] 
              ), 
            Variable("4") ] 
          )
        ) 
      )
    ) 
    (* Result *)
    ( 
      true, 
      Unifier([ 
        Binding( 
          Term( Functor("8"), [] ), 
          Variable("9") 
        ), 
        Binding( 
          Variable("4"), 
          Term( Functor("3"), [] ) 
        ), 
        Binding( 
          Term( Functor("6"), [] ), 
          Variable("5") 
        ) 
      ]) 
    )
    (* Equality test *)
    eqTupleUnifier;
    
    (* ------------------------------- *)
    
    printResultPoly "unify 12" 
    (* Test *)
    (
      unify ( 
        Unifier([ 
          Binding( 
            Term( Functor("8"), [] ), 
            Variable("9") ) 
        ])
      ) 
      ( 
        Binding( 
          Variable("1"), 
          Variable("2") 
        ) 
      )
    ) 
    (* Result *)
    ( 
      true, 
      Unifier([ 
        Binding( 
          Term( Functor("8"), [] ), 
          Variable("9") 
        ), 
        Binding( 
          Variable("1"), 
          Variable("2") 
        ) 
      ]) 
    )
    (* Equality test *)
    eqTupleUnifier;
    
    (* ------------------------------- *)
    
    (* Single term, empty unifier, successful unification *)
    printResult "findUnifier 1" ( findUnifier ( Term( Functor( "green" ), [] ) )
            ( Unifier([]) ) ( fn x => fn y => op= ( Unifier([]), x ) ) 
            ( fn () => false ) ) true;
    
    (* Single term, empty unifier, failed unification *)
    printResult "findUnifier 2" ( findUnifier ( Term( Functor( "blue" ), [] ) )
            ( Unifier([]) ) ( fn x => fn y => false ) 
            ( fn () => true ) ) true;
    
    (* Single term, non-empty unifier *)
    printResult "findUnifier 3" ( findUnifier ( Term( Functor( "green" ), [] ) )
            ( Unifier([ 
              Binding(
                Variable("2"), 
                Variable("3")
              )
            ]) )
            ( fn x => fn y => ( eqUnifier ( Unifier( [ 
              Binding(
                Variable("2"), 
                Variable("3")
              ) ] ), x ) ) ) 
            ( fn () => true ) ) true;
    
    (* Variable, empty unifier *)
    printResult "findUnifier 4" ( findUnifier ( Variable("1") )
            ( Unifier([]) ) 
            ( fn x => fn y => ( eqUnifier ( Unifier( [ Binding(
              Variable("1"), 
              Term( Functor( "green" ), [] )
            ) ] ), x ) ) ) 
            ( fn () => false ) ) true;
	
    (* Variable, non-empty unifier *)
    printResult "findUnifier 5" ( findUnifier ( Variable("1") )
            ( Unifier([ 
              Binding(
                Variable("2"), 
                Variable("3")
              )
            ]) ) 
            (* k1 *)
            ( fn x => fn y => ( eqUnifier ( Unifier( [ 
              Binding(
                Variable("2"), 
                Variable("3")
              ), 
              Binding(
                Variable("1"), 
                Term( Functor( "green" ), [] )
              ) ] ), x ) ) ) 
            (* k2 *)
            ( fn () => false ) ) true;
    
    (* Variable, reject and retry *)
    printResult "findUnifier 6" ( findUnifier ( Variable("1") )
            ( Unifier([]) ) 
            ( fn x => fn y => ( 
              if eqUnifier ( 
                Unifier([ 
                  Binding(
                    Variable("1"), 
                    Term( Functor( "green" ), [] )
                  ) 
                ]), 
                x 
              ) 
                then y() 
              else eqUnifier ( 
                Unifier([ 
                  Binding(
                    Variable("1"), 
                    Term( Functor( "red" ), [] )
                  ) 
                ]), 
                x 
              )
            ) ) 
            ( fn () => false ) ) true;
    
    (* Clause with condition, direct input *)
    printResult "findUnifier 7" ( findUnifier 
            ( Term( Functor( "likes" ), [
              Term( Functor( "pooh" ), [] ), 
              Term( Functor( "honey" ), [] )
            ] ) )
            ( Unifier([]) ) ( fn x => fn y => op= ( Unifier([]), x ) ) 
            ( fn () => false ) ) true;
    
    (* Clause with condition, variable input, success *)
    printResult "findUnifier 8" ( findUnifier 
            ( Term( Functor( "likes" ), [
              Variable("1"), 
              Variable("2")
            ] ) )
            ( Unifier([]) ) 
            ( fn x => fn y => op= ( Unifier([
              Binding(
                Variable("1"),
                Term( Functor( "pooh" ), [] )
              ),
              Binding(
                Variable("2"),
                Term( Functor( "honey" ), [] )
              )
            ]), x ) ) 
            ( fn () => false ) ) true;
    
    (* Clause with condition, direct input, failure on second term of clause 
       body *)
    printResult "findUnifier 9" ( findUnifier 
            ( Term( Functor( "purple" ), [] ) )
            ( Unifier([]) ) 
            ( fn x => fn y => false) 
            ( fn () => true ) ) true;
	
    (* Two query terms, both succeed. *)
    printResult "executeQuery 1" ( 
      executeQuery ( 
        Query([ 
          Term( Functor( "green" ), [] ), 
          Term( Functor( "red" ), [] ) 
        ]) 
      )
      ( fn x => eqUnifier ( Unifier([]), x ) )
      ( fn() => false )
    ) true;
	
    (* Two query terms, second fails. *)
    printResult "executeQuery 2" ( 
      executeQuery ( 
        Query([ 
          Term( Functor( "green" ), [] ), 
          Term( Functor( "blue" ), [] ) 
        ]) 
      )
      ( fn x => false )
      ( fn() => true )
    ) true;
	
    (* Variable query term. *)
    printResult "executeQuery 3" ( 
      executeQuery ( 
        Query([ 
          Term( Functor( "bear" ), [ Variable("1") ] )
        ]) 
      )
      ( fn x => eqUnifier ( Unifier([
        Binding(
          Variable("1"),
          Term( Functor( "pooh" ), [] )
        )
      ]), x ) )
      ( fn() => false )
    ) true;
    
	conclude()
  )
	
  end end end end end;
  