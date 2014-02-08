datatype functor_t = Functor of string;
datatype term_t = Term of functor_t * term_t list
                | Variable of string * int
                | IntTerm of int
                | FloatTerm of real;
datatype clause_t = Clause of term_t * term_t list;
datatype program_t = Program of clause_t list;
datatype query_t = Query of term_t list;
datatype binding_t = Binding of term_t * term_t;
datatype unifier_t = Unifier of binding_t list;

val scopeCounter = ref 2;

(* Takes a list and a value. Returns true if the value is in the list. *)
fun memberPoly x [] _ = false
  | memberPoly x (y::ys) eqTest = eqTest(x, y) orelse memberPoly x ys eqTest;

(* Takes a list and a value. Returns true if the value is in the list. *)
fun member x ys = memberPoly x ys op=;
  
(* Takes two lists, and removes from the first any elements that occur in the 
   second. *)
fun removeWithBlacklist [] ys = []
  | removeWithBlacklist (x::xs) ys = 
        if ( member x ys ) 
		    then removeWithBlacklist xs ys 
	    else (x::( removeWithBlacklist xs ys ));

(* Takes two lists of equal length, and returns a list of 2-tuples, where the 
   i'th tuple contains the i'th element of each input list. *)
fun zip [] [] = []
  | zip (x::xs) (y::ys) = (x,y) :: (zip xs ys);

(* Takes two lists of equal length, and returns a list of 2-tuples, where the 
   i'th tuple contains the i'th element of each input list. *)
fun zipBinding [] [] = []
  | zipBinding (x::xs) (y::ys) = Binding(x,y) :: (zipBinding xs ys);

(* Return the first and second elements of an input 2-tuple, respectively. *)
fun first (a, b) = a;
fun second (a, b) = b;

(* Takes a list of bools and returns the result of ANDing them all together. *)
fun andList [] = true
  | andList (x::xs) = x andalso andList xs;

(* Takes a 2-tuple and returns the a 2-tuple with the same elements in reverse 
   order. *)
fun flip( x, y ) = ( y, x );

(* Takes a binding and returns another Binding with the elements in reverse 
   order. *)
fun flipBinding ( Binding( x, y ) ) = Binding( y, x );

(* Polymorphic equality test for ordered lists *)
fun eqOrderedList _ ( [], [] ) = true
  | eqOrderedList _ ( xs, [] ) = false
  | eqOrderedList _ ( [], ys ) = false
  | eqOrderedList eqTest ( (x::xs), (y::ys) ) = 
        eqTest( x, y ) andalso eqOrderedList eqTest ( xs, ys );

(* Equality test for Terms *)
fun eqTerm( Term( f1, args1 ), Term( f2, args2 ) ) = 
        ( f1 = f2 ) andalso ( eqOrderedList eqTerm ( args1, args2 ) )
  | eqTerm( Variable( v1, s1 ), Variable( v2, s2 ) ) =
        ( v1 = v2 ) andalso ( s1 = s2 )
  | eqTerm( IntTerm( i1 ), IntTerm( i2 ) ) = ( i1 = i2 )
  | eqTerm( FloatTerm( f1 ), FloatTerm( f2 ) ) = Real.==( f1, f2 )
  | eqTerm( term1, term2 ) = false;
  
(*fun op=( Term( ( f1 : functor_t ), args1 ), Term( ( f2 : functor_t ), args2 ) ) = ( op=( f1, f2 ) ) andalso ( args1 = args2 );*)

(* Equality test for Bindings *)
fun eqBinding( Binding( term1A, term1B ), Binding( term2A, term2B ) ) = 
        ( eqTerm( term1A, term2A ) andalso eqTerm( term1B, term2B ) )
          orelse ( eqTerm( term1A, term2B ) andalso eqTerm( term1B, term2A ) );
		  
(* Polymorphic equality test for 2-tuples *)
fun eqTwoTuple eqTestA eqTestB ( ( x1, y1 ), (x2, y2 ) ) = 
        eqTestA( x1, x2 ) andalso eqTestB( y1, y2 );
		  
(* Polymorphic equality test for unordered lists. *)
fun eqUnorderedList _ ( [], [] ) = true
  | eqUnorderedList _ ( [], ys ) = false
  | eqUnorderedList _ ( xs, [] ) = false
  | eqUnorderedList eqTest ( (x::xs), ys ) = 
    (* For each element of the first list, step through the second list and 
	   match it to an equal element. Remove both elements and repeat. *)
    let fun remove( x, [] ) = ( false, [] )
	      | remove( x, (y::ys) ) = 
		        if( eqTest(x, y) )
			        then ( true, ys )
				else
				    let val result = remove( x, ys ) in
					    if( first result )
						    then ( true, (y::( second result )) )
					    else result 
					end in
	let val result = remove( x, ys ) in
        (first result) andalso eqUnorderedList eqTest ( xs, ( second result ) )
	end end;

(* Equality test for Unifiers *)
fun eqUnifier ( Unifier(xs), Unifier(ys) ) = eqUnorderedList eqBinding ( xs, ys );

(* Equality test for Clauses *)
fun eqClause( Clause(head1, body1), Clause(head2, body2) ) = 
        eqTerm( head1, head2 ) andalso eqOrderedList eqTerm ( body1, body2 );

fun printTerm ( Variable(name, scope) ) = ( 
        if( not(scope = 0) andalso not(scope = 1) )
            then ( print ( Int.toString( scope ) ); print "_" )
        else
            ();
        print name )
  | printTerm ( Term( Functor( func ), terms ) ) = 
    let fun printListTerm( Term(_, [term, Variable(v,s)] ) ) = 
                ( printTerm( term ); print "|"; printTerm( Variable(v,s) ) )
          | printListTerm( Term(_, [term, Term( Functor( f ), args )] ) ) = (
                printTerm( term );
                if( f = "." )
                    then ( print ", "; 
                           printListTerm( Term( Functor( f ), args ) ) )
                else (*if( f = "[]" )*)
                    ()
                )
    in
        ( 
            if( func = "." )
                then ( print "["; 
                       printListTerm( Term( Functor( func ), terms ) );
                       print "]" )
            else
                ( print func; 
                if( not( null terms ) )
                    then ( 
                        print "(";
                        printTerms terms;
                        print ")"
                    )
                else
                    () )
        )
    end
  | printTerm( IntTerm( i ) ) = print ( Int.toString( i ) )
  | printTerm( FloatTerm( f ) ) = print ( Real.toString( f ) )

and printTerms [] = ()
  | printTerms [term] = printTerm term
  | printTerms (term::terms) = ( 
        printTerm term; 
        print ", ";
        printTerms terms
    );
        
fun printQuery ( Query(terms) ) = 
    let fun printTerms [] = ()
          | printTerms [term] = printTerm term
          | printTerms (term::terms) = ( 
                printTerm ( term ); 
                print ", ";
                printTerms( terms ) 
            ) in
        printTerms terms 
    end;
    
fun printAllBindings [] = ()
  | printAllBindings (binding::bindings) = 
    let fun printBinding ( Binding( Variable( name, scope ), term2 ) ) = (
        printTerm ( Variable( name, scope ) );
        print " = ";
        printTerm term2
       ) 
         | printBinding binding = printBinding ( flipBinding binding ) 
    in
        (
            printBinding binding;
            print "\n";
            printAllBindings bindings
        )
    end;
    
(* Only outputs variable-to-term Bindings, and only for variables in the 
   original query, ie: scope "1". *)
fun printCoreBindings [] = ()
  | printCoreBindings (binding::bindings) = 
    let fun printBinding( Binding( Variable( _, _ ), Variable( _, _ ) ) ) = ()
          | printBinding( Binding( Variable( name, scope ), term ) ) =
        if( scope = 1 ) then (
            printTerm ( Variable( name, scope ) );
            print " = ";
            printTerm ( term );
            print "\n"
        )
        else 
            ()
          | printBinding binding = printBinding ( flipBinding binding ) 
    in
        (
            printBinding binding;
            printCoreBindings bindings
        )
    end;
    
fun printUnifier ( Unifier(bindings) ) = printCoreBindings bindings;

(* Takes two Bindings and returns a (bool, Binding) tuple. If they share a 
   common term, then the first value is true, and the second value is the 
   Binding containing the two distinct terms (ie: the Binding that exists by 
   transitivity). If they do not share a common term, or if the Bindings are 
   equal, then the first value is false. *)
fun getTransitiveBinding ( Binding( term1A, term1B ) ) 
            ( Binding( term2A, term2B ) ) = 
		if( eqBinding( ( Binding( term1A, term1B ) ), 
		        ( Binding( term2A, term2B ) ) ) )
			then ( false, Binding( term1A, term2A ) )
	    else if( eqTerm( term1A, term2A ) )
		    then ( true, Binding( term1B, term2B ) )
		else if( eqTerm( term1A, term2B ) )
		    then ( true, Binding( term1B, term2A ) )
		else if( eqTerm( term1B, term2A ) )
		    then ( true, Binding( term1A, term2B ) )
		else if( eqTerm( term1B, term2B ) )
		    then ( true, Binding( term1A, term2A ) )
		else ( false, Binding( term1A, term2A ) );
		
(* Takes a binding x and a list of bindings. Returns all transitive bindings 
   that are implied by adding x to the list. *)
fun getAllTransitiveBindings x [] = []
  | getAllTransitiveBindings x (y::ys) = 
    let val transitive = getTransitiveBinding x y in
		if ( first transitive )
		    then ( ( second transitive ) :: ( getAllTransitiveBindings x ys ) )
		else
		    getAllTransitiveBindings x ys
	end;

(* Takes a list of Bindings and returns the transitive closure. *)
fun getTransitiveClosure xs = 
    (* Start with an empty list, add each input Binding one by one, adding the 
	   transitive links each time. *)
    let fun getTransitiveClosureWorker [] ys = ys
          | getTransitiveClosureWorker (x::xs) ys = 
            if ( memberPoly x ys eqBinding )
		        then getTransitiveClosureWorker xs ys
		    else
		        getTransitiveClosureWorker xs 
				        ( (x::ys) @ ( getAllTransitiveBindings x ys ) ) in 
		getTransitiveClosureWorker xs []
	end;
	
(* Takes a list of Unifiers and returns a list of all the Bindings that occur 
   in them, potentially with duplicates. *)
fun combineUnifiers [] = []
  | combineUnifiers ((Unifier(xs))::ys) = xs @ ( combineUnifiers ys );

(* Takes a Unifier and a Binding and returns a 2-tuple. If the binding is 
   consistent with itself and with the input Unifier, the first return value is 
   "true" and the second is an updated Unifier that satisfies the input Binding.
   If the binding is not consistent, the first return value is "false" and the 
   second return value is undefined. *)
fun unify ( Unifier(defaultBindings) ) 
        ( Binding( Term( funcA, argsA ), Term( funcB, argsB ) ) ) = 
		
    (* Takes a list of Bindings and returns true if the Bindings are consistent.
    *)
    let fun consistentBindings [] = true
          | consistentBindings (x::xs) = 
                first ( unify (Unifier([])) x ) andalso consistentBindings xs in

    (* Takes a list of bindings and returns a (bool, Unifier) tuple. If the 
       bindings are consistent, then the first value is true and the second 
	   value is a Unifier containing the bindings. If the bindings are 
	   inconsistent, then the first value is false. *)
    let fun validateUnifier xs = 
        let val transitiveClosure = getTransitiveClosure xs in
	        if( consistentBindings transitiveClosure )
		        then ( true, Unifier( transitiveClosure ) )
		    else
		        ( false, Unifier( [] ) )
	    end in

	(* check that the functors and arities match *)
	let val successOfThis = 
	    ( funcA = funcB ) andalso 
	    ( length( argsA ) = length( argsB ) ) in
	if( not successOfThis ) then ( false, Unifier([]) ) else
    (* unify the arguments pairwise *)
    let val unificationOfArgs = 
            map ( unify ( Unifier([]) ) ) ( zipBinding argsA argsB ) in
	(* check that all the arguments unified successfully *)
	let val successOfArgs = andList ( map first unificationOfArgs ) in
	if( not successOfArgs ) then ( false, Unifier([]) ) else 
	(* verify that the resulting unifier is consistent *)
	let val rawBindings = combineUnifiers( 
            ( Unifier(defaultBindings) )::( map second unificationOfArgs ) ) in
	let val finalUnifier = validateUnifier rawBindings in
	if ( first finalUnifier )
	    then ( true, ( second finalUnifier ) )
	else
	    ( false, Unifier([]) )
	end end end end end end end
  | unify ( Unifier(xs) ) ( Binding( IntTerm(i1), IntTerm(i2) ) ) =
        if( i1 = i2 )
            then ( true, Unifier(xs) )
        else
            ( false, Unifier([]) )
  | unify ( Unifier(xs) ) ( Binding( FloatTerm(f1), FloatTerm(f2) ) ) =
        if( Real.==( f1, f2 ) )
            then ( true, Unifier(xs) )
        else
            ( false, Unifier([]) )
  (* Reject incompatible cross-combinations of terms. *)
  | unify _ ( Binding( Term(_), IntTerm(_) ) ) = ( false, Unifier([]) )
  | unify _ ( Binding( IntTerm(_), Term(_) ) ) = ( false, Unifier([]) )
  | unify _ ( Binding( Term(_), FloatTerm(_) ) ) = ( false, Unifier([]) )
  | unify _ ( Binding( FloatTerm(_), Term(_) ) ) = ( false, Unifier([]) )
  | unify _ ( Binding( IntTerm(_), FloatTerm(_) ) ) = ( false, Unifier([]) )
  | unify _ ( Binding( FloatTerm(_), IntTerm(_) ) ) = ( false, Unifier([]) )
  (* Remaining cases must involve a variable. *)
  | unify ( Unifier([]) ) x = ( true, Unifier( [x] ) )
  | unify ( Unifier(defaultBindings) ) ( Binding( x, y ) ) = 
    (* Takes a list of Bindings and returns true if the Bindings are consistent.
    *)
    let fun consistentBindings [] = true
          | consistentBindings (x::xs) = 
                first ( unify (Unifier([])) x ) andalso consistentBindings xs in

    (* Takes a list of bindings and returns a (bool, Unifier) tuple. If the 
       bindings are consistent, then the first value is true and the second 
	   value is a Unifier containing the bindings. If the bindings are 
	   inconsistent, then the first value is false. *)
    let fun validateUnifier xs = 
        let val transitiveClosure = getTransitiveClosure xs in
	        if( consistentBindings transitiveClosure )
		        then ( true, Unifier( transitiveClosure ) )
		    else
		        ( false, Unifier( [] ) )
	    end 
    in
        validateUnifier( ( Binding( x, y ) ):: defaultBindings ) 
        
    end end;

(* Takes a Clause and a scope id. Returns a clause whose variables have the 
   given scope id. *)
fun scopeClause( Clause( head, body ), scope ) = 
    let fun scopeTerms [] = []
          | scopeTerms (term::terms) = 
            let fun scopeTerm ( Term( f, args ) ) = 
                        Term( f, ( scopeTerms args ) )
                  | scopeTerm ( Variable( v, _ ) ) = 
                        Variable( v, scope )
                  | scopeTerm ( IntTerm(i) ) = IntTerm(i)
                  | scopeTerm ( FloatTerm(f) ) = FloatTerm(f)
            in
                ( scopeTerm( term ) ) :: ( scopeTerms( terms ) )
            end
    in
        Clause( ( hd( scopeTerms( [ head ] ) ) ), scopeTerms( body ) )                        
    end;

(* Takes a Query and updates all variables occurring in it to have scope "1". *)
fun scopeQuery( Query(xs) ) = 
    let fun scopeTerms [] = []
          | scopeTerms (term::terms) = 
            let fun scopeTerm ( Term( f, args ) ) = 
                        Term( f, ( scopeTerms args ) )
                  | scopeTerm ( Variable( v, _ ) ) = 
                        Variable( v, 1 )
                  | scopeTerm ( IntTerm(i) ) = IntTerm(i)
                  | scopeTerm ( FloatTerm(f) ) = FloatTerm(f)
            in
                ( scopeTerm( term ) ) :: ( scopeTerms( terms ) )
            end
    in
        Query( scopeTerms( xs ) )                        
    end;
    
fun evaluate( Variable(v) ) = Variable(v)
  | evaluate( IntTerm(i) ) = IntTerm(i)
  | evaluate( FloatTerm(f) ) = FloatTerm(f)
  | evaluate( Term( Functor(f), [arg1, arg2] ) ) = 
    let fun addTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 + i2 )
          | addTerms( IntTerm(i1), FloatTerm(f2) ) = FloatTerm( real(i1) + f2 )
          | addTerms( FloatTerm(f1), IntTerm(i2) ) = FloatTerm( f1 + real(i2) )
          | addTerms( FloatTerm(f1), FloatTerm(f2) ) = FloatTerm( f1 + f2 )
    in
    let fun subTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 - i2 )
          | subTerms( IntTerm(i1), FloatTerm(f2) ) = FloatTerm( real(i1) - f2 )
          | subTerms( FloatTerm(f1), IntTerm(i2) ) = FloatTerm( f1 - real(i2) )
          | subTerms( FloatTerm(f1), FloatTerm(f2) ) = FloatTerm( f1 - f2 )
    in
    let fun mulTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 * i2 )
          | mulTerms( IntTerm(i1), FloatTerm(f2) ) = FloatTerm( real(i1) * f2 )
          | mulTerms( FloatTerm(f1), IntTerm(i2) ) = FloatTerm( f1 * real(i2) )
          | mulTerms( FloatTerm(f1), FloatTerm(f2) ) = FloatTerm( f1 * f2 )
    in
    let fun divTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 div i2 )
          | divTerms( IntTerm(i1), FloatTerm(f2) ) = FloatTerm( real(i1) / f2 )
          | divTerms( FloatTerm(f1), IntTerm(i2) ) = FloatTerm( f1 / real(i2) )
          | divTerms( FloatTerm(f1), FloatTerm(f2) ) = FloatTerm( f1 / f2 )
    in
    let fun modTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 mod i2 )
    in
    if( f = "+" )
        then addTerms( evaluate( arg1 ), evaluate( arg2 ) )
    else if( f = "-" )
        then subTerms( evaluate( arg1 ), evaluate( arg2 ) )
    else if( f = "*" )
        then mulTerms( evaluate( arg1 ), evaluate( arg2 ) )
    else if( f = "/" )
        then divTerms( evaluate( arg1 ), evaluate( arg2 ) )
    else if( f = "%" )
        then modTerms( evaluate( arg1 ), evaluate( arg2 ) )
    else Term( Functor(f), [arg1, arg2] )
    end end end end end
  | evaluate( Term( f, args ) ) = Term( f, args );
  
(* Returns true if the input is a Term with functor "is", false otherwise. *)
fun functorIs( Term( Functor( f ), [arg1, arg2] ) ) = ( f = "is" )
  | functorIs( term ) = false;
    
(* Takes a term, an input Unifier and two continuations. If the term is 
   satisfiable by the (hardcoded) Prolog program, in a way that is consistent 
   with the input Unifier, then the first continuation is called with the 
   updated Unifier. If not, then the second continuation is called with unit. *)
fun findUnifier program term unifier k1 k2 = 
    (* Takes a list of terms and finds a unifier that satisfies all of them. *)
    let fun findUnifiers [] unifier k1 k2 = k1 unifier k2
          | findUnifiers (term::terms) unifier k1 k2 = 
            let fun m1 newUnifier k3 = findUnifiers terms newUnifier k1 k3
            in 
                findUnifier program term unifier m1 k2
            end in
    let fun worker [] = k2()
          | worker ( ( clause as Clause( _, _ ) )::clauses ) = 
            let val scope = !scopeCounter in 
            let val liveClause as Clause( head, body ) = 
                        scopeClause( clause, scope ) in 
            let val incScope = ( scopeCounter := !scopeCounter + 1 ) in
            (* If the term is an "is" term, evaluate its args. *)
            if( functorIs( term ) ) then 
                let val x as Term( _, [arg1, arg2] ) = term in
                let val unification = unify unifier 
                            ( Binding( evaluate( arg1 ), evaluate( arg2 ) ) )
                in
                    if( first unification ) then 
                        k1 ( second unification ) k2
                    else
                        k2()
                end end
            (* Or in the general case, run the unification algorithm. *)
            else
                let val unification = unify unifier ( Binding( term, head ) )
                in
                    if ( first unification )
                        then let fun m2() = worker clauses
                             in 
                                 findUnifiers body ( second unification ) k1 m2
                             end
                    else
                        worker clauses
                end 
            end end end in
    let fun getClauses ( Program(xs) ) = xs in
        worker ( getClauses program )
    end end end;

(* Takes a program, a query and two continuations. If the query is satisfiable 
   by the program, then the first continuation is called with the most general 
   unifier that satisfies it. If not, then the second continuation is called 
   with unit. *)
fun executeQuery program (Query(xs)) k1 k2 = 
    let fun executeQueryTerms [] unifier k1 _ = k1 unifier
          | executeQueryTerms (x::xs) unifier k1 k2 = 
            let fun m1 newUnifier k = executeQueryTerms xs newUnifier k1 k
            in
                findUnifier program x unifier m1 k2
            end
    in
        executeQueryTerms xs (Unifier([])) k1 k2
    end;
    
(* Takes two term-to-variable bindings, and applies the first to the second as a
   substitution, returning the updated second binding. Returns the binding 
   unchanged if either binding is variable-to-variable. *)
(* If either Binding is purely variables, then there is no change. *)
fun substitute ( Binding( Variable( _, _ ), Variable( _, _ ) ) ) b2 = b2
  | substitute b1 ( Binding( Variable( v1, s1 ), Variable( v2, s2 ) ) ) = 
        ( Binding( Variable( v1, s1 ), Variable( v2, s2 ) ) )
(* Mirror bindings if the variable comes before the term. *)
  | substitute ( Binding( Variable( v1, s1 ), t1 ) ) b2 = 
        substitute ( Binding( t1, Variable( v1, s1 ) ) ) b2
  | substitute b1 ( Binding( Variable( v2, s2 ), t2 ) ) = 
        substitute b1 ( Binding( t2, Variable( v2, s2 ) ) )
(* No substitution if the target is an IntTerm or a FloatTerm binding *)
  | substitute b1 ( Binding( IntTerm(i), Variable(v,s) ) ) = 
        Binding( IntTerm(i), Variable(v,s) )
  | substitute b1 ( Binding( FloatTerm(f), Variable(v,s) ) ) = 
        Binding( FloatTerm(f), Variable(v,s) )
(* Main version when the target is a Term: *)
  | substitute ( Binding( term, Variable( v1, s1 ) ) ) 
            ( Binding( Term( f2, args2 ), Variable( v2, s2 ) ) ) =
    let fun worker ( Term( f, args ) ) = 
            let fun iterateArgs [] = []
                  | iterateArgs (arg::remaining) = 
                        ( worker arg ) :: ( iterateArgs remaining )
            in
                Term( f, ( iterateArgs args ) )
            end
          | worker ( Variable( v, s ) ) = 
                if( eqTerm( Variable( v, s ), Variable( v1, s1 ) ) )
                    then term
                else Variable( v, s )
          | worker ( IntTerm(i) ) = IntTerm(i)
          | worker ( FloatTerm(f) ) = FloatTerm(f)
    in
        Binding( ( worker ( Term( f2, args2 ) ) ), Variable( v2, s2 ) )
    end;

(* Takes a consistent Unifier with no term-to-term Bindings. Expands Bindings 
   containing Terms by replacing Variables with Terms according to other 
   Bindings in the Unifier. *)
fun substituteUnifier ( Unifier(xs) ) = 
    let fun addBinding ( Binding( x, y ) ) bindings = 
        let fun firstRound newBinding [] = newBinding
              | firstRound newBinding ( nextBinding::remaining ) = 
                    firstRound ( substitute nextBinding newBinding ) remaining
        in
        let fun secondRound newBinding [] = [ newBinding ]
              | secondRound newBinding ( nextBinding::remaining ) = 
                    ( substitute newBinding nextBinding )::
                            ( secondRound newBinding remaining )
        in
            secondRound ( firstRound ( Binding( x, y ) ) bindings ) bindings
        end end in
    let fun worker [] ys = Unifier(ys)
          | worker (x::xs) ys = worker xs ( addBinding x ys )
    in
        worker xs []
    end end;

(* Takes a program and a list of queries. Returns true if all of the queries 
   can be satisfied by the program. *)
fun executeQueries( _, [] ) = true
  | executeQueries( program, (x::xs) ) = 
    let fun m1 unifier = ( 
            printQuery x; 
            print "\n";
            let val subUnifier = substituteUnifier unifier in
            printUnifier ( subUnifier ) 
            end;
            executeQueries( program, xs) 
    ) in
    let fun m2() = ( 
            printQuery x; 
            print "\nQuery not satisfiable."; 
            false 
    ) in
        executeQuery program (scopeQuery x) m1 m2
    end end;
