fun unitTestInterpreter() = 
  let val testProgram = Program( [
    Clause( Term( Functor( "green" ), [] ), [] ), 
    Clause( Term( Functor( "red" ), [] ), [] ), 
    Clause( 
      Term( 
        Functor( "bear" ), 
        [
          Term( Functor( "pooh" ), [] )
        ] 
      ), 
      [] 
    ), 
    Clause( 
      Term( 
        Functor( "likes" ), 
        [
          Term( Functor( "pooh" ), [] ), 
          Term( Functor( "honey" ), [] )
        ] 
      ), 
      [
        Term( 
          Functor( "bear" ), 
          [
            Term( Functor( "pooh" ), [] )
          ] 
        )
      ]
    ),
    Clause( Term( Functor( "purple" ), [] ), [Term( Functor( "red" ), [] ), Term( Functor( "blue" ), [] )] )
    ] ) in
  let val testQueries = [
    Query([
      Term( Functor( "bear" ), [Variable( "_G1675", 0 )] ), 
      Term( 
        Functor( "likes" ), 
        [
          Variable( "_G1675", 0 ), 
          Term( Functor( "honey" ), [] )
        ] 
      )
    ])
  ] 
  in ( 
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
    UnitTester.printResult "removeWithBlacklist 2"( removeWithBlacklist [] [1,2,3] ) [];
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
	UnitTester.printResultPoly "flipBinding" ( 
	  flipBinding ( Binding( Variable("1",0), Variable("2",0) ) ) 
	) (
	  Binding( Variable("2",0), Variable("1",0) )
    )
    eqBinding;
	UnitTester.printResultPoly "getTransitiveBinding 1" (
	  getTransitiveBinding ( Binding( Variable("1",0), Variable("2",0) ) )
	  ( Binding( Variable("2",0), Variable("3",0) ) ) 
	) 
    ( 
      true, ( Binding( Variable("1",0), Variable("3",0) ) ) 
    )
    ( eqTwoTuple op= eqBinding );
	UnitTester.printResultPoly "getTransitiveBinding 2" (
	  getTransitiveBinding ( Binding( Variable("2",0), Variable("1",0) ) )
	    ( Binding( Variable("2",0), Variable("3",0) ) )
	) ( true, ( Binding( Variable("1",0), Variable("3",0) ) ) )
    ( eqTwoTuple op= eqBinding );
	UnitTester.printResultPoly "getTransitiveBinding 3" (
	  getTransitiveBinding ( Binding( Variable("1",0), Variable("2",0) ) )
	    ( Binding( Variable("3",0), Variable("2",0) ) ) 
	) ( true, ( Binding( Variable("1",0), Variable("3",0) ) ) )
    ( eqTwoTuple op= eqBinding );
	UnitTester.printResultPoly "getTransitiveBinding 4" (
	  getTransitiveBinding ( Binding( Variable("2",0), Variable("1",0) ) )
	    ( Binding( Variable("3",0), Variable("2",0) ) )
	) ( true, ( Binding( Variable("1",0), Variable("3",0) ) ) )
    ( eqTwoTuple op= eqBinding );
	UnitTester.printResult "getTransitiveBinding 5" (
	  first( 
	    getTransitiveBinding ( Binding( Variable("1",0), Variable("2",0) ) )
	      ( Binding( Variable("3",0), Variable("4",0) ) )
      )
	) false;
	UnitTester.printResult "getTransitiveBinding 6" (
	  first( 
	    getTransitiveBinding ( Binding( Variable("1",0), Variable("2",0) ) )
	      ( Binding( Variable("2",0), Variable("1",0) ) )
      )
	) false;
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
	UnitTester.printResultPoly "getAllTransitiveBindings 1" (
	  getAllTransitiveBindings ( Binding( Variable("1",0), Variable("2",0) ) )
	      [ ( Binding( Variable("2",0), Variable("3",0) ) ),
		    ( Binding( Variable("3",0), Variable("4",0) ) ),
		    ( Binding( Variable("4",0), Variable("2",0) ) ) ]
	) [ ( Binding( Variable("1",0), Variable("3",0) ) ), 
	    ( Binding( Variable("1",0), Variable("4",0) ) ) 
	  ] ( eqUnorderedList eqBinding );
	UnitTester.printResultPoly "getAllTransitiveBindings 2" (
	  getAllTransitiveBindings ( Binding( Variable("1",0), Variable("2",0) ) )
	      []
	) [] ( eqUnorderedList eqBinding );
	UnitTester.printResultPoly "getAllTransitiveBindings 3" (
	  getAllTransitiveBindings ( Binding( Variable("1",0), Variable("5",0) ) )
	      [ ( Binding( Variable("2",0), Variable("3",0) ) ),
		    ( Binding( Variable("3",0), Variable("4",0) ) ),
		    ( Binding( Variable("4",0), Variable("2",0) ) ) ]
	) [] ( eqUnorderedList eqBinding );
	
	UnitTester.printResultPoly "getTransitiveClosure 1" (
	  getTransitiveClosure [ ( Binding( Variable("1",0), Variable("2",0) ) ),
	                         ( Binding( Variable("3",0), Variable("2",0) ) ),
	                         ( Binding( Variable("2",0), Variable("4",0) ) ),
	                         ( Binding( Variable("5",0), Variable("7",0) ) ),
	                         ( Binding( Variable("7",0), Variable("6",0) ) ) ]
	) [ ( Binding( Variable("1",0), Variable("2",0) ) ),
	    ( Binding( Variable("1",0), Variable("3",0) ) ),
	    ( Binding( Variable("1",0), Variable("4",0) ) ),
	    ( Binding( Variable("2",0), Variable("3",0) ) ),
	    ( Binding( Variable("2",0), Variable("4",0) ) ),
	    ( Binding( Variable("3",0), Variable("4",0) ) ),
	    ( Binding( Variable("5",0), Variable("6",0) ) ),
	    ( Binding( Variable("5",0), Variable("7",0) ) ),
	    ( Binding( Variable("6",0), Variable("7",0) ) ) ]
		  ( eqUnorderedList eqBinding );
		  
    UnitTester.printResultPoly "getTransitiveClosure 2" (
	  getTransitiveClosure [ ( Binding( Variable("1",0), Variable("2",0) ) ),
	                         ( Binding( Variable("1",0), Variable("3",0) ) ),
	                         ( Binding( Variable("1",0), Variable("4",0) ) ),
	                         ( Binding( Variable("2",0), Variable("3",0) ) ),
	                         ( Binding( Variable("2",0), Variable("4",0) ) ),
	                         ( Binding( Variable("3",0), Variable("4",0) ) ),
	                         ( Binding( Variable("5",0), Variable("6",0) ) ),
	                         ( Binding( Variable("5",0), Variable("7",0) ) ),
	                         ( Binding( Variable("6",0), Variable("7",0) ) ) ]
	) [ ( Binding( Variable("1",0), Variable("2",0) ) ),
	    ( Binding( Variable("1",0), Variable("3",0) ) ),
	    ( Binding( Variable("1",0), Variable("4",0) ) ),
	    ( Binding( Variable("2",0), Variable("3",0) ) ),
	    ( Binding( Variable("2",0), Variable("4",0) ) ),
	    ( Binding( Variable("3",0), Variable("4",0) ) ),
	    ( Binding( Variable("5",0), Variable("6",0) ) ),
	    ( Binding( Variable("5",0), Variable("7",0) ) ),
	    ( Binding( Variable("6",0), Variable("7",0) ) ) ]
		  ( eqUnorderedList eqBinding );
		  
    UnitTester.printResultPoly "getTransitiveClosure 3" (
	  getTransitiveClosure []
	) [] ( eqUnorderedList eqBinding );
	
    UnitTester.printResultPoly "combineUnifiers 1" 
      ( combineUnifiers [] ) [] ( eqUnorderedList eqBinding );
    
    UnitTester.printResultPoly "combineUnifiers 2" ( 
      combineUnifiers [ 
        Unifier([ Binding( Variable("1",0), Variable("2",0) ) ]) 
      ] 
    ) [ Binding( Variable("1",0), Variable("2",0) ) ] ( eqUnorderedList eqBinding );
    
    UnitTester.printResultPoly "combineUnifiers 3" ( 
      combineUnifiers [ 
        Unifier([ Binding( Variable("1",0), Variable("2",0) ), 
                  Binding( Variable("3",0), Variable("4",0) ) ]),
        Unifier([ Binding( Variable("2",0), Variable("3",0) ) ])
      ]
    ) [ Binding( Variable("1",0), Variable("2",0) ), 
        Binding( Variable("3",0), Variable("4",0) ),
        Binding( Variable("2",0), Variable("3",0) ) ]
      ( eqUnorderedList eqBinding );
    
    UnitTester.printResultPoly "unify 1" (
      unify (Unifier([])) ( Binding( Variable("1",0), Variable("2",0) ) )
    ) ( true, Unifier([ Binding( Variable("1",0), Variable("2",0) ) ]) ) 
    ( eqTwoTuple op= eqUnifier );
    
    UnitTester.printResultPoly "unify 2" (
      unify (Unifier([])) 
        ( Binding( Term( Functor("1"), [] ), Variable("2",0) ) )
    ) 
    ( true, Unifier([ Binding( Term( Functor("1"), [] ), Variable("2",0) ) ]) )
    ( eqTwoTuple op= eqUnifier );
    
    UnitTester.printResultPoly "unify 3" (
      unify (Unifier([])) ( Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ), 
        Variable("2",0) 
      ) )
    ) ( true, Unifier([ Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ), 
        Variable("2",0) 
      ) ]) )
    ( eqTwoTuple op= eqUnifier );
    
    UnitTester.printResultPoly "unify 4" (
      unify (Unifier([])) ( Binding( 
        Variable("2",0), 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] )
      ) )
    ) ( true, Unifier([ Binding( 
        Variable("2",0), 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ) 
      ) ]) )
    ( eqTwoTuple op= eqUnifier );
    
    UnitTester.printResultPoly "unify 5" (
      unify (Unifier([])) ( Binding( 
        Term( Functor("1"), [] ),
        Term( Functor("1"), [] )
      ) )
    ) ( true, Unifier([]) )
    ( eqTwoTuple op= eqUnifier );
    
    UnitTester.printResult "unify 6" (
      first( unify (Unifier([])) ( Binding( 
        Term( Functor("1"), [] ),
        Term( Functor("2"), [] )
      ) ) )
    ) false;
    
    UnitTester.printResultPoly "unify 7" (
      unify (Unifier([])) ( Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ),
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] )
      ) )
    ) ( true, Unifier([]) )
    ( eqTwoTuple op= eqUnifier );
    
    UnitTester.printResult "unify 8" (
      first( unify (Unifier([])) ( Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("3"), [] ) ] ),
        Term( Functor("1"), 
          [ Term( Functor("2"), [] ), Term( Functor("4"), [] ) ] )
      ) ) )
    ) false;
    
    UnitTester.printResultPoly "unify 9" (
      unify (Unifier([])) ( Binding( 
        Term( Functor("1"), 
          [ Term( Functor("2"), 
            [ Variable("5",0) ] ), 
          Term( Functor("3"), [] ) ] ),
        Term( Functor("1"), 
          [ Term( Functor("2"), 
            [ Term( Functor("6"), [] ) ] ), 
          Variable("4",0) ] )
      ) )
    ) ( true, Unifier([ Binding( 
        Variable("4",0), 
        Term( Functor("3"), [] ) 
      ), Binding( 
        Term( Functor("6"), [] ), 
        Variable("5",0) 
      ) ]) )
    ( eqTwoTuple op= eqUnifier );
    
    UnitTester.printResultPoly "unify 10" 
    (* Test *)
    (
      unify (
        Unifier([ 
          Binding( 
            Term( Functor("6"), [] ), 
            Variable("5",0) 
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
                [ Variable("5",0) ] 
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
            Variable("4",0) ] 
          )
        ) 
      )
    ) 
    (* Result *)
    ( 
      true, 
      Unifier([ 
        Binding( 
          Variable("4",0), 
          Term( Functor("3"), [] ) 
        ), 
        Binding( 
          Term( Functor("6"), [] ), 
          Variable("5",0) 
        ) 
      ]) 
    )
    (* Equality test *)
    ( eqTwoTuple op= eqUnifier );
    
    (* ------------------------------- *)
    
    UnitTester.printResultPoly "unify 11" 
    (* Test *)
    (
      unify (
        Unifier([ 
          Binding( 
            Term( Functor("8"), [] ), 
            Variable("9",0) 
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
                [ Variable("5",0) ] 
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
            Variable("4",0) ] 
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
          Variable("9",0) 
        ), 
        Binding( 
          Variable("4",0), 
          Term( Functor("3"), [] ) 
        ), 
        Binding( 
          Term( Functor("6"), [] ), 
          Variable("5",0) 
        ) 
      ]) 
    )
    (* Equality test *)
    ( eqTwoTuple op= eqUnifier );
    
    (* ------------------------------- *)
    
    UnitTester.printResultPoly "unify 12" 
    (* Test *)
    (
      unify ( 
        Unifier([ 
          Binding( 
            Term( Functor("8"), [] ), 
            Variable("9",0) ) 
        ])
      ) 
      ( 
        Binding( 
          Variable("1",0), 
          Variable("2",0) 
        ) 
      )
    ) 
    (* Result *)
    ( 
      true, 
      Unifier([ 
        Binding( 
          Term( Functor("8"), [] ), 
          Variable("9",0) 
        ), 
        Binding( 
          Variable("1",0), 
          Variable("2",0) 
        ) 
      ]) 
    )
    (* Equality test *)
    ( eqTwoTuple op= eqUnifier );
    
    (* ------------------------------- *)
    
    (* Single term, empty unifier, successful unification *)
    UnitTester.printResult "findUnifier 1" ( findUnifier testProgram ( Term( Functor( "green" ), [] ) )
            ( Unifier([]) ) ( fn x => fn y => eqUnifier ( Unifier([]), x ) ) 
            ( fn () => false ) ) true;
    
    (* Single term, empty unifier, failed unification *)
    UnitTester.printResult "findUnifier 2" ( findUnifier testProgram ( Term( Functor( "blue" ), [] ) )
            ( Unifier([]) ) ( fn x => fn y => false ) 
            ( fn () => true ) ) true;
    
    (* Single term, non-empty unifier *)
    UnitTester.printResult "findUnifier 3" ( findUnifier testProgram ( Term( Functor( "green" ), [] ) )
            ( Unifier([ 
              Binding(
                Variable("2",0), 
                Variable("3",0)
              )
            ]) )
            ( fn x => fn y => ( eqUnifier ( Unifier( [ 
              Binding(
                Variable("2",0), 
                Variable("3",0)
              ) ] ), x ) ) ) 
            ( fn () => true ) ) true;
    
    (* Variable, empty unifier *)
    UnitTester.printResult "findUnifier 4" ( findUnifier 
            testProgram 
            ( Variable("1",0) )
            ( Unifier([]) ) 
            ( fn x => fn y => ( eqUnifier ( Unifier( [ Binding(
              Variable("1",0), 
              Term( Functor( "green" ), [] )
            ) ] ), x ) ) ) 
            ( fn () => false ) ) true;
	
    (* Variable, non-empty unifier *)
    UnitTester.printResult "findUnifier 5" ( findUnifier 
            testProgram 
            ( Variable("1",0) )
            ( Unifier([ 
              Binding(
                Variable("2",0), 
                Variable("3",0)
              )
            ]) ) 
            (* k1 *)
            ( fn x => fn y => ( eqUnifier ( Unifier( [ 
              Binding(
                Variable("2",0), 
                Variable("3",0)
              ), 
              Binding(
                Variable("1",0), 
                Term( Functor( "green" ), [] )
              ) ] ), x ) ) ) 
            (* k2 *)
            ( fn () => false ) ) true;
    
    (* Variable, reject and retry *)
    UnitTester.printResult "findUnifier 6" ( findUnifier 
            testProgram 
            ( Variable("1",0) )
            ( Unifier([]) ) 
            ( fn x => fn y => ( 
              if eqUnifier ( 
                Unifier([ 
                  Binding(
                    Variable("1",0), 
                    Term( Functor( "green" ), [] )
                  ) 
                ]), 
                x 
              ) 
                then y() 
              else eqUnifier ( 
                Unifier([ 
                  Binding(
                    Variable("1",0), 
                    Term( Functor( "red" ), [] )
                  ) 
                ]), 
                x 
              )
            ) ) 
            ( fn () => false ) ) true;
    
    (* Clause with condition, direct input *)
    UnitTester.printResult "findUnifier 7" ( findUnifier 
            testProgram 
            ( Term( Functor( "likes" ), [
              Term( Functor( "pooh" ), [] ), 
              Term( Functor( "honey" ), [] )
            ] ) )
            ( Unifier([]) ) ( fn x => fn y => eqUnifier ( Unifier([]), x ) ) 
            ( fn () => false ) ) true;
    
    (* Clause with condition, variable input, success *)
    UnitTester.printResult "findUnifier 8" ( findUnifier 
            testProgram 
            ( Term( Functor( "likes" ), [
              Variable("1",0), 
              Variable("2",0)
            ] ) )
            ( Unifier([]) ) 
            ( fn x => fn y => eqUnifier( Unifier([
              Binding(
                Variable("1",0),
                Term( Functor( "pooh" ), [] )
              ),
              Binding(
                Variable("2",0),
                Term( Functor( "honey" ), [] )
              )
            ]), x ) ) 
            ( fn () => false ) ) true;
    
    (* Clause with condition, direct input, failure on second term of clause 
       body *)
    UnitTester.printResult "findUnifier 9" ( findUnifier 
            testProgram 
            ( Term( Functor( "purple" ), [] ) )
            ( Unifier([]) ) 
            ( fn x => fn y => false) 
            ( fn () => true ) ) true;
	
    (* Two query terms, both succeed. *)
    UnitTester.printResult "executeQuery 1" ( 
      executeQuery testProgram ( 
        Query([ 
          Term( Functor( "green" ), [] ), 
          Term( Functor( "red" ), [] ) 
        ]) 
      )
      ( fn x => eqUnifier ( Unifier([]), x ) )
      ( fn() => false )
    ) true;
	
    (* Two query terms, second fails. *)
    UnitTester.printResult "executeQuery 2" ( 
      executeQuery testProgram ( 
        Query([ 
          Term( Functor( "green" ), [] ), 
          Term( Functor( "blue" ), [] ) 
        ]) 
      )
      ( fn x => false )
      ( fn() => true )
    ) true;
	
    (* Variable query term. *)
    UnitTester.printResult "executeQuery 3" ( 
      executeQuery testProgram ( 
        Query([ 
          Term( Functor( "bear" ), [ Variable("1",0) ] )
        ]) 
      )
      ( fn x => eqUnifier ( Unifier([
        Binding(
          Variable("1",0),
          Term( Functor( "pooh" ), [] )
        )
      ]), x ) )
      ( fn() => false )
    ) true;
    
    (* ---------------------------------------------------------------------- *)
    
    (* B1( V, V ), B2( V, V ) *)
    UnitTester.printResultPoly "substitute 1" ( 
      substitute (
        Binding( Variable("1",0), Variable("2",0) )
      )
      (
        Binding( Variable("3",0), Variable("4",0) )
      )
    )
    (
      Binding( Variable("3",0), Variable("4",0) )
    )
    eqBinding;
    
    (* B1( T, V ), B2( V, V ) *)
    UnitTester.printResultPoly "substitute 2" ( 
      substitute (
        Binding( Term( Functor("1"), [] ), Variable("2",0) )
      )
      (
        Binding( Variable("3",0), Variable("4",0) )
      )
    )
    (
      Binding( Variable("3",0), Variable("4",0) )
    )
    eqBinding;
    
    (* B1( V, V ), B2( T, V ) *)
    UnitTester.printResultPoly "substitute 3" ( 
      substitute (
        Binding( Variable("1",0), Variable("2",0) )
      )
      (
        Binding( Term( Functor("3"), [] ), Variable("4",0) )
      )
    )
    (
      Binding( Term( Functor("3"), [] ), Variable("4",0) )
    )
    eqBinding;
    
    (* No substitution *)
    UnitTester.printResultPoly "substitute 4" ( 
      substitute (
        Binding( Term( Functor("1"), [] ), Variable("2",0) )
      )
      (
        Binding( Term( Functor("3"), [] ), Variable("4",0) )
      )
    )
    (
      Binding( Term( Functor("3"), [] ), Variable("4",0) )
    )
    eqBinding;
    
    (* Substitutions *)
    UnitTester.printResultPoly "substitute 5" ( 
      substitute (
        Binding( Term( Functor("1"), [] ), Variable("2",0) )
      )
      (
        Binding( 
          Term( 
            Functor("3"), 
            [
              Variable("2",0), 
              Variable("4",0), 
              Variable("2",0)
            ]
          ), 
          Variable("5",0) 
        )
      )
    )
    (
      Binding( 
          Term( 
            Functor("3"), 
            [
              Term( Functor("1"), [] ), 
              Variable("4",0), 
              Term( Functor("1"), [] )
            ]
          ), 
          Variable("5",0) 
        )
    )
    eqBinding;
    
    (* Potential (incorrect) reverse substitution *)
    UnitTester.printResultPoly "substitute 6" ( 
      substitute (
        Binding( 
          Term( 
            Functor("1"), 
            [
              Variable("4",0), 
              Variable("5",0), 
              Variable("4",0)
            ]
          ), 
          Variable("2",0) 
        )
      )
      (
        Binding( Term( Functor("3"), [] ), Variable("4",0) )
      )
    )
    (
      Binding( Term( Functor("3"), [] ), Variable("4",0) )
    )
    eqBinding;
    
    (* ---------------------------------------------------------------------- *)
    
    (* Empty Unifier *)
    UnitTester.printResultPoly "substituteUnifier 1" (
      substituteUnifier ( Unifier([]) )
    )
    (
      Unifier([])
    )
    eqUnifier;
    
    (* Single Binding *)
    UnitTester.printResultPoly "substituteUnifier 2" (
      substituteUnifier ( Unifier([
        Binding( Variable("1",0), Variable("2",0) ) 
      ]) )
    )
    (
      Unifier([ Binding( Variable("1",0), Variable("2",0) ) ])
    )
    eqUnifier;
    
    (* Two independent Bindings *)
    UnitTester.printResultPoly "substituteUnifier 3" (
      substituteUnifier ( Unifier([
        Binding( Variable("1",0), Variable("2",0) ) ,
        Binding( Variable("3",0), Variable("4",0) )
      ]) )
    )
    (
      Unifier([
        Binding( Variable("1",0), Variable("2",0) ) ,
        Binding( Variable("3",0), Variable("4",0) )
      ])
    )
    eqUnifier;
    
    (* Two dependent Bindings *)
    UnitTester.printResultPoly "substituteUnifier 4" (
      substituteUnifier ( Unifier([
        Binding( Term( Functor("1"), [ Variable("2",0) ] ), Variable("3",0) ) ,
        Binding( Term( Functor("4"), [] ), Variable("2",0) )
      ]) )
    )
    (
      Unifier([
        Binding( 
          Term( Functor("1"), [ Term( Functor("4"), [] ) ] ), 
          Variable("3",0) 
        ) ,
        Binding( Term( Functor("4"), [] ), Variable("2",0) )
      ])
    )
    eqUnifier;
    
    (* ---------------------------------------------------------------------- *)
    
    (* Simple fact clause *)
    UnitTester.printResultPoly "scopeClause 1" (
      scopeClause( Clause( Term( Functor("One"), [] ), [] ), 1 )
    )
    (
      Clause( Term( Functor("One"), [] ), [] )
    )
    eqClause;
    
    (* Fact clause containing variables *)
    UnitTester.printResultPoly "scopeClause 2" (
      scopeClause( Clause( Term( Functor("1"), [
        Variable("2",0), Variable("3",0)
      ] ), [] ), 1 )
    )
    (
      Clause( Term( Functor("1"), [
        Variable("2",1), Variable("3",1)
      ] ), [] )
    )
    eqClause;
    
    (* Clause with two conditions *)
    UnitTester.printResultPoly "scopeClause 3" (
      scopeClause( Clause( Term( Functor("1"), [
        Variable("2",0), Variable("3",0)
      ] ), [
        Term( Functor("4"), [
          Variable("5",0),
          Variable("6",0)
        ] ),
        Variable("7",0)
      ] ), 1 )
    )
    (
      Clause( Term( Functor("1"), [
        Variable("2",1), Variable("3",1)
      ] ), [
        Term( Functor("4"), [
          Variable("5",1),
          Variable("6",1)
        ] ),
        Variable("7",1)
      ] )
    )
    eqClause
  )
	
  end end;
  