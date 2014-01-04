datatype functor_t = Functor of string;
datatype term_t = Term of functor_t * term_t list
                | Variable of string;
datatype clause_t = Clause of term_t * term_t list;
datatype program_t = Program of clause_t list;
datatype query_t = Query of term_t list;
datatype binding_t = Binding of term_t * term_t;
datatype unifier_t = Unifier of binding_t list;

val program = Program( [
Clause( Term( Functor( "bear" ), [Term( Functor( "pooh" ), [] )] ), [] ), 
Clause( Term( Functor( "likes" ), [Term( Functor( "pooh" ), [] ), Term( Functor( "honey" ), [] )] ), [] ), 
Clause( Term( Functor( "likes" ), [Term( Functor( "pooh" ), [] ), Term( Functor( "honey" ), [] )] ), [Term( Functor( "bear" ), [Term( Functor( "pooh" ), [] )] ), Term( Functor( "real" ), [Term( Functor( "bear" ), [] )] ), Term( Functor( "my" ), [Term( Functor( "term" ), [] )] )] )
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

(* Takes a Binding and returns a 2-tuple. If the binding is consistent, the first return value is "true" and the second is a Unifier that satisfies it. If the binding is not consistent, the first return value is "false" and the second return value is (...?) *)
fun unify ( Binding( Term( funcA, argsA ), Term( funcB, argsB ) ) ) = 
		
    (* Takes a list of Bindings and returns true if the Bindings are consistent. *)
    let fun consistentBindings [] = true
          | consistentBindings (x::xs) = 
                first ( unify x ) andalso consistentBindings xs in

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
    let val unificationOfArgs = map unify ( zipBinding argsA argsB ) in
	(* check that all the arguments unified successfully *)
	let val successOfArgs = andList ( map first unificationOfArgs ) in
	if( not successOfArgs ) then ( false, Unifier([]) ) else 
	(* verify that the resulting unifier is consistent *)
	let val rawBindings = combineUnifiers( map second unificationOfArgs ) in
	let val finalUnifier = validateUnifier rawBindings in
	if ( first finalUnifier )
	    then ( true, ( second finalUnifier ) )
	else
	    ( false, Unifier([]) )
	end end end end end end end
  | unify ( Binding( Term( func, args ), Variable( name ) ) ) =
        ( true, Unifier([ ( Binding( Variable( name ), Term( func, args ) ) ) ]) )
  | unify ( Binding( Variable( name ), Term( func, args ) ) ) = 
        unify( Binding( Term( func, args ), Variable( name ) ) )
  | unify ( Binding( Variable( name1 ), Variable( name2 ) ) ) =
        ( true, Unifier([ ( Binding( Variable( name1 ), Variable( name2 ) ) ) ]) );
