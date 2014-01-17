datatype functor_t = Functor of string;
datatype term_t = Term of functor_t * term_t list
                | Variable of string;
datatype clause_t = Clause of term_t * term_t list;
datatype program_t = Program of clause_t list;
datatype query_t = Query of term_t list;
datatype binding_t = Binding of term_t * term_t;
datatype unifier_t = Unifier of binding_t list;

val program = Program( [
Clause( Term( Functor( "green" ), [] ), [] ), 
Clause( Term( Functor( "red" ), [] ), [] ), 
Clause( Term( Functor( "bear" ), [Term( Functor( "pooh" ), [] )] ), [] ), 
Clause( Term( Functor( "likes" ), [Term( Functor( "pooh" ), [] ), Term( Functor( "honey" ), [] )] ), [Term( Functor( "bear" ), [Term( Functor( "pooh" ), [] )] )] ),
Clause( Term( Functor( "purple" ), [] ), [Term( Functor( "red" ), [] ), Term( Functor( "blue" ), [] )] )
] );
val queries = [Query ( [Term( Functor( "bear" ), [Variable( "_G1675" )] ), Term( Functor( "likes" ), [Variable( "_G1675" ), Term( Functor( "honey" ), [] )] )] )];

  
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

(* Equality test for Bindings *)
fun eqBinding( x, y ) = (x = y) orelse ( x = (flipBinding y) ); 

(* Equality test for (''a, Binding) tuples *)
fun eqTupleBinding( x, y ) = 
        ( (first x) = (first y) ) andalso 
          ( eqBinding( ( second x ), (second y) ) ) ; 
		  
(* Equality test for lists of Bindings. Lists with the same elements in a 
   different order are considered equal. *)
fun eqUnorderedBindingList( [], [] ) = true
  | eqUnorderedBindingList( [], ys ) = false
  | eqUnorderedBindingList( xs, [] ) = false
  | eqUnorderedBindingList( (x::xs), ys ) = 
    (* For each element of the first list, step through the second list and 
	   match it to an equal element. Remove both elements and repeat. *)
    let fun remove( x, [] ) = ( false, [] )
	      | remove( x, (y::ys) ) = 
		        if( eqBinding(x, y) )
			        then ( true, ys )
				else
				    let val result = remove( x, ys ) in
					    if( first result )
						    then ( true, (y::( second result )) )
					    else result 
					end in
	let val result = remove( x, ys ) in
        (first result) andalso eqUnorderedBindingList( xs, ( second result ) )
	end end;

(* Equality test for Unifiers *)
fun eqUnifier ( Unifier(xs), Unifier(ys) ) = eqUnorderedBindingList( xs, ys );

(* Equality test for (''a, Unifier) tuples *)
fun eqTupleUnifier ( (a,b), (c,d) ) = (a=c) andalso ( eqUnifier( b, d ) );

fun printTerm ( Variable(name) ) = print name
  | printTerm ( Term( Functor( func ), terms ) ) = 
    let fun printTerms [] = ()
          | printTerms [term] = printTerm term
          | printTerms (term::terms) = ( 
                printTerm term; 
                print ", ";
                printTerms terms
            ) in 
        ( 
            print func; 
            if( not( null terms ) )
                then ( 
                    print "(";
                    printTerms terms;
                    print ")"
                )
            else
                ()
        )
    end;
        
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
    
fun printBindings [] = ()
  | printBindings (binding::bindings) = 
    let fun printBinding ( Binding( Variable( name ), term2 ) ) = (
        print name;
        print " = ";
        printTerm term2
       ) 
         | printBinding binding = printBinding ( flipBinding binding ) 
    in
        (
            printBinding binding;
            print "\n";
            printBindings bindings
        )
    end;
    
fun printUnifier ( Unifier(bindings) ) = printBindings bindings;

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
	    else if( term1A = term2A )
		    then ( true, Binding( term1B, term2B ) )
		else if( term1A = term2B )
		    then ( true, Binding( term1B, term2A ) )
		else if( term1B = term2A )
		    then ( true, Binding( term1A, term2B ) )
		else if( term1B = term2B )
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

(* Takes a term, an input Unifier and two continuations. If the term is 
   satisfiable by the (hardcoded) Prolog program, in a way that is consistent 
   with the input Unifier, then the first continuation is called with the 
   updated Unifier. If not, then the second continuation is called with unit. *)
fun findUnifier term unifier k1 k2 = 
    (* Takes a list of terms and finds a unifier that satisfies all of them. *)
    let fun findUnifiers [] unifier k1 k2 = k1 unifier k2
          | findUnifiers (term::terms) unifier k1 k2 = 
            let fun m1 newUnifier k3 = findUnifiers terms newUnifier k1 k3
            in findUnifier term unifier m1 k2
            end in

    let fun worker [] = k2()
          | worker ( ( Clause( head, body ) )::clauses ) = 
            let val unification = unify unifier ( Binding( term, head ) )
            in
                if ( first unification )
                    then let fun m1() = worker clauses
                         in findUnifiers body ( second unification ) k1 m1 end
                    else
                        worker clauses
            end in
    let fun getClauses ( Program(xs) ) = xs in
        worker ( getClauses program )
    end end end;

(* Takes a query and two continuations. If the query is satisfiable, then the 
   first continuation is called with the most general unifier that satisfies it.
   If not, then the second continuation is called with unit. *)
fun executeQuery (Query(xs)) k1 k2 = 
    let fun executeQueryTerms [] unifier k1 _ = k1 unifier
          | executeQueryTerms (x::xs) unifier k1 k2 = 
            let fun m1 newUnifier k = executeQueryTerms xs newUnifier k1 k
            in
                findUnifier x unifier m1 k2
            end
    in
        executeQueryTerms xs (Unifier([])) k1 k2
    end;
    
(* Takes a list of queries. Returns true if all of the queries can be 
   satisfied. *)
fun executeQueries [] = true
  | executeQueries (x::xs) = 
    let fun m1 unifier = ( 
            printQuery x; 
            print "\n"; 
            printUnifier unifier; 
            executeQueries xs 
    ) in
    let fun m2() = ( 
            printQuery x; 
            print "\nQuery not satisfiable."; 
            false 
    ) in
        executeQuery x m1 m2
    end end;

