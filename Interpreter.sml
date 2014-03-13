(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the functions for interpreting a Prolog program and 
finding a Unifier, if any, that satisfies a query.
*******************************************************************************)

val scopeCounter = ref 2;

(* Takes two lists of equal length, and returns a list of 2-tuples, where the 
   i'th tuple contains the i'th element of each input list. *)
fun zipBinding [] [] = []
  | zipBinding (x::xs) (y::ys) = Binding(x,y) :: (zipBinding xs ys);
  
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
  | combineUnifiers( ( Unifier(xs) )::ys ) = xs @ ( combineUnifiers( ys ) );
  
(* Takes a list of Bindings and returns true if the Bindings are consistent, 
   along with the expanded list of Bindings which includes those Bindings which 
   are necessary to make the input Bindings unify. *)
fun consistentBindings [] = ( true, [] )
  | consistentBindings( bindings ) = 
    let fun worker( [], results ) = ( true, results )
          | worker( (binding::bindings), results ) = 
            let val x as (success, result) = unify ( Unifier([]) ) binding
            in
                if( success ) then 
                    worker( bindings, (result::results) )
                else
                    ( false, [] )
            end in
    let val x as (success, unifierList) = worker( bindings, [] )
    in
        if( success ) then
            ( true, ( combineUnifiers( unifierList ) ) )
        else
            ( false, [] )
    end end
  
(* Takes a list of bindings and returns a (bool, Unifier) tuple. If the bindings
   are consistent, then the first value is true and the second value is a 
   Unifier containing the bindings. If the bindings are inconsistent, then the 
   first value is false. *)
and validateUnifier( bindings ) = 
    (* Takes the transitive closure of the bindings *)
    let val transitiveClosure = getTransitiveClosure( bindings ) in
    (* Test whether the bindings are consistent, and generate the Unifier that 
       makes them consistent. *)
    let val x as (success, iteratedBindings) = 
            consistentBindings( transitiveClosure )
    in
        if( success ) then
            (* If no new Bindings were needed, we're done. *)
            if( eqUnorderedListIgnoreDups eqBinding 
                    ( iteratedBindings, bindings ) ) then
                ( true, Unifier( transitiveClosure ) )
            (* If new Bindings were needed, we need to validate them too. *)
            else
                validateUnifier( iteratedBindings )
		else
		    ( false, Unifier( [] ) )
    end end

(* Takes a Unifier and a Binding and returns a 2-tuple. If the binding is 
   consistent with itself and with the input Unifier, the first return value is 
   "true" and the second is an updated Unifier that satisfies the input Binding.
   If the binding is not consistent, the first return value is "false" and the 
   second return value is undefined. *)
and unify ( Unifier(defaultBindings) ) 
        ( Binding( Term( funcA, argsA ), Term( funcB, argsB ) ) ) = 
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
	end end end end end
    
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
  | unify ( Unifier([]) ) ( Binding( x, y ) ) = 
        ( true, Unifier( [ Binding( x,y ) ] ) )
  | unify ( Unifier(defaultBindings) ) ( Binding( x, y ) ) = 
        validateUnifier( ( Binding( x, y ) ):: defaultBindings );

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

exception UninstantiatedVariable;
    
(* Takes a variable and a unifier. Scans the unifier and returns the term to 
   which the variable is bound. Raises an expression if no binding is found. *)
fun subVarForTerm( var, unifier ) = 
    let fun getBindings( Unifier(xs) ) = xs in
    let fun worker( [] ) = raise UninstantiatedVariable
            (* Variable-variable bindings are definitely not helpful. *)
          | worker( Binding( Variable(_, _), Variable(_, _) )::bindings ) =
                worker( bindings )
            (* If we find a binding for the variable we want to sub, then sub 
               it! *)
          | worker( Binding( Variable( v, s ), term )::bindings ) = 
                if( eqTerm( Variable( v, s ), var ) ) then
                    term
                else
                    worker( bindings )
            (* If the binding is backwards, flip it. *)
          | worker( Binding( term, Variable( v, s ) )::bindings ) = 
                worker( Binding( Variable( v, s ), term )::bindings )
            (* Any other binding should be skipped. *)
          | worker( binding::bindings ) = worker( bindings )
    in
        worker( getBindings( unifier ) )
    end end;

(* Takes a term and arithmetically evaluates it. *)
fun evaluate( Variable(v, s), unifier ) = 
        evaluate( subVarForTerm( Variable( v, s ), unifier ), unifier )
  | evaluate( IntTerm(i), _ ) = IntTerm(i)
  | evaluate( FloatTerm(f), _ ) = FloatTerm(f)
  | evaluate( Term( Functor(f), [arg1, arg2] ), unifier ) = 
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
        then addTerms( evaluate( arg1, unifier ), evaluate( arg2, unifier ) )
    else if( f = "-" )
        then subTerms( evaluate( arg1, unifier ), evaluate( arg2, unifier ) )
    else if( f = "*" )
        then mulTerms( evaluate( arg1, unifier ), evaluate( arg2, unifier ) )
    else if( f = "/" )
        then divTerms( evaluate( arg1, unifier ), evaluate( arg2, unifier ) )
    else if( f = "%" )
        then modTerms( evaluate( arg1, unifier ), evaluate( arg2, unifier ) )
    else Term( Functor(f), [arg1, arg2] )
    end end end end end
  | evaluate( Term( f, args ), _ ) = Term( f, args );
    
fun specialPredicateAtomic( Term( _, [ Term( _, [] ) ] ), unifier, k1, k2 ) = 
        k1 unifier k2
  | specialPredicateAtomic( Term( _, [ IntTerm(_) ] ), unifier, k1, k2 ) = 
        k1 unifier k2
  | specialPredicateAtomic( Term( _, [ FloatTerm(_) ] ), unifier, k1, k2 ) = 
        k1 unifier k2
  | specialPredicateAtomic( Term( t, [ Variable( v,s ) ] ), unifier, k1, k2 ) = 
        let val result = ( specialPredicateAtomic( 
          Term( t, [ subVarForTerm( Variable( v, s ), unifier ) ] ), 
            unifier, k1, k2 ) )
          handle UninstantiatedVariable => ( k2() ) 
        in 
            result
        end
  | specialPredicateAtomic( term, _, _, k2 ) = k2();
    
fun specialPredicateIs( Term( _, [ t1, t2 ] ), unifier, k1, k2 ) = 
    let val evaluation = evaluate( t2, unifier ) in
    let val x as (success, newUnifier) = 
            unify unifier ( Binding( t1, evaluation ) )
    in
        if( success ) then
            k1 newUnifier k2
        else
            k2()
    end end;
    
fun specialPredicateGreater( 
      Term( _, [ IntTerm(i1), IntTerm(i2) ] ), unifier, k1, k2 ) =
        if( i1 > i2 ) then
            k1 unifier k2
        else
            k2()
  | specialPredicateGreater( 
      Term( _, [ IntTerm(i1), FloatTerm(f2) ] ), unifier, k1, k2 ) =
        if( real(i1) > f2 ) then
            k1 unifier k2
        else
            k2()
  | specialPredicateGreater( 
      Term( _, [ FloatTerm(f1), IntTerm(i2) ] ), unifier, k1, k2 ) =
        if( f1 > real(i2) ) then
            k1 unifier k2
        else
            k2()
  | specialPredicateGreater( 
      Term( _, [ FloatTerm(f1), FloatTerm(f2) ] ), unifier, k1, k2 ) =
        if( f1 > f2 ) then
            k1 unifier k2
        else
            k2()
  | specialPredicateGreater( 
      Term( f, [ Variable( v, s ), term ] ), unifier, k1, k2 ) =
        specialPredicateGreater( 
          Term( f, [ subVarForTerm( Variable( v, s ), unifier ), term ] ), 
            unifier, k1, k2 )
  | specialPredicateGreater( 
      Term( f, [ term, Variable( v, s ) ] ), unifier, k1, k2 ) =
        specialPredicateGreater( 
          Term( f, [ term, subVarForTerm( Variable( v, s ), unifier ) ] ), 
            unifier, k1, k2 );
    
fun specialPredicateLess( 
      Term( _, [ IntTerm(i1), IntTerm(i2) ] ), unifier, k1, k2 ) =
        if( i1 < i2 ) then
            k1 unifier k2
        else
            k2()
  | specialPredicateLess( 
      Term( _, [ IntTerm(i1), FloatTerm(f2) ] ), unifier, k1, k2 ) =
        if( real(i1) < f2 ) then
            k1 unifier k2
        else
            k2()
  | specialPredicateLess( 
      Term( _, [ FloatTerm(f1), IntTerm(i2) ] ), unifier, k1, k2 ) =
        if( f1 < real(i2) ) then
            k1 unifier k2
        else
            k2()
  | specialPredicateLess( 
      Term( _, [ FloatTerm(f1), FloatTerm(f2) ] ), unifier, k1, k2 ) =
        if( f1 < f2 ) then
            k1 unifier k2
        else
            k2()
  | specialPredicateLess( 
      Term( f, [ Variable( v, s ), term ] ), unifier, k1, k2 ) =
        specialPredicateLess( 
          Term( f, [ subVarForTerm( Variable( v, s ), unifier ), term ] ), 
            unifier, k1, k2 )
  | specialPredicateLess( 
      Term( f, [ term, Variable( v, s ) ] ), unifier, k1, k2 ) =
        specialPredicateLess( 
          Term( f, [ term, subVarForTerm( Variable( v, s ), unifier ) ] ), 
            unifier, k1, k2 );
          
    
(* Takes a term, a unifier and three continuations. If the term is one of the 
   special built-in predicates, then it is evaluated by a special-purpose 
   function. If it succeeds, k1 is called with the new unifier and with k3 as a 
   backtracking continuation. If it fails, then the backtracking continuation k3
   is called. If the term is not a special predicate, then k2 is called to 
   evaluate it the normal way. *)
fun specialPredicate ( IntTerm(i) ) _ _ k2 _ = k2()
  | specialPredicate ( FloatTerm(i) ) _ _ k2 _ = k2()
  | specialPredicate ( Variable(i) ) _ _ k2 _ = k2()
  | specialPredicate ( Term( Functor( f ), args ) ) unifier k1 k2 k3 = 
        if( ( f = "atomic" ) andalso ( List.length( args ) = 1 ) ) then
            specialPredicateAtomic( Term( Functor(f), args ), unifier, k1, k3 )
        else if( ( f = "is" ) andalso ( List.length( args ) = 2 ) ) then
            specialPredicateIs( Term( Functor(f), args ), unifier, k1, k3 )
        else if( ( f = ">" ) andalso ( List.length( args ) = 2 ) ) then
            specialPredicateGreater( Term( Functor(f), args ), unifier, k1, k3 )
        else if( ( f = "<" ) andalso ( List.length( args ) = 2 ) ) then
            specialPredicateLess( Term( Functor(f), args ), unifier, k1, k3 )
        else
            k2();
        
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
            let val unification = unify unifier ( Binding( term, head ) )
            in
                if ( first unification ) then 
                    let fun m2() = worker clauses
                        in 
                            findUnifiers body ( second unification ) k1 m2
                        end
                else
                    worker clauses
            end end end end in
    let fun getClauses ( Program(xs) ) = xs in
    (* Make a continuation for running through the clauses. *)
    let fun m1() = worker ( getClauses program )
    in
        (* But first check if we're dealing with a special predicate. *)
        specialPredicate term unifier k1 m1 k2
    end end end end;
    
(* Takes a program, a query and two continuations. If the query is satisfiable 
   by the program, then the first continuation is called with the most general 
   unifier that satisfies it. If not, then the second continuation is called 
   with unit. *)
fun executeQuery program (Query(xs)) k1 k2 = 
    let fun executeQueryTerms [] unifier k1 _ = k1 unifier
          | executeQueryTerms (x::xs) unifier k1 k2 = 
            (* Executes the next term. *)
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

(* Takes a Unifier and returns a Unifier containing the same Bindings, excluding
   those which don't include a variable. *)
fun cleanUnifier( Unifier(xs) ) =
    let fun worker( [] ) = []
          | worker( ( Binding( Term( _, _ ), Term( _, _ ) ) )::bindings ) = 
                worker( bindings )
          | worker( ( Binding( IntTerm( _ ), IntTerm( _ ) ) )::bindings ) = 
                worker( bindings )
          | worker( ( Binding( FloatTerm( _ ), FloatTerm( _ ) ) )::bindings ) = 
                worker( bindings )
          | worker( ( Binding( term1, term2 ) )::bindings ) = 
                ( Binding( term1, term2 ) )::( worker( bindings ) )
    in
        Unifier( worker xs )
    end;

(* Takes a program and a list of queries. Returns true if all of the queries 
   can be satisfied by the program. *)
fun executeQueries( _, [] ) = true
  | executeQueries( program, (x::xs) ) = 
    let fun m1 unifier = ( 
            printQuery x; 
            print "\n";
            (* Remove bindings which don't feature a variable. *)
            let val reducedUnifier = cleanUnifier unifier in
            (* Perform variable substitutions. *)
            let val subUnifier = substituteUnifier reducedUnifier in
                (* Print the Unifier. *)
                printUnifier ( subUnifier ) 
            end end;
            executeQueries( program, xs) 
    ) in
    let fun m2() = ( 
            printQuery x; 
            print "\nQuery not satisfiable.\n"; 
            false 
    ) in
        executeQuery program x m1 m2
    end end;
