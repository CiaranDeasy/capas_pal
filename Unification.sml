val scopeCounter = ref 2;

(* Takes two lists of equal length, and returns a list of 2-tuples, where the 
   i'th tuple contains the i'th element of each input list. *)
fun zipBinding( [], [] ) = []
  | zipBinding( (x::xs), (y::ys) ) = Binding(x,y)::( zipBinding( xs, ys ) );
  
(* Takes two Bindings and returns a (bool, Binding) tuple. If they share a 
   common term, then the first value is true, and the second value is the 
   Binding containing the two distinct terms (ie: the Binding that exists by 
   transitivity). If they do not share a common term, or if the Bindings are 
   equal, then the first value is false. *)
fun getTransitiveBinding( Binding( term1A, term1B ), 
            Binding( term2A, term2B ) ) = 
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
fun getAllTransitiveBindings( x, [] ) = []
  | getAllTransitiveBindings( x, (y::ys) ) = 
    let val transitive = getTransitiveBinding( x, y ) in
        if( first( transitive ) ) then 
            ( second( transitive ) ) :: ( getAllTransitiveBindings( x, ys ) )
        else
            getAllTransitiveBindings( x, ys )
    end;

(* Takes a list of Bindings and returns the transitive closure. *)
fun getTransitiveClosure( xs ) = 
    (* Start with an empty list, add each input Binding one by one, adding the 
       transitive links each time. *)
    let fun getTransitiveClosureWorker( [], ys ) = ys
          | getTransitiveClosureWorker( (x::xs), ys ) = 
            if( memberPoly( x, ys, eqBinding ) )
                then getTransitiveClosureWorker( xs, ys )
            else
                getTransitiveClosureWorker( xs, 
                        ( (x::ys) @ ( getAllTransitiveBindings( x, ys ) ) ) ) in
        getTransitiveClosureWorker( xs, [] )
    end;
    
(* Takes a list of Unifiers and returns a list of all the Bindings that occur 
   in them, potentially with duplicates. *)
fun combineUnifiers( [] ) = []
  | combineUnifiers( ( Unifier(xs) )::ys ) = xs @ ( combineUnifiers( ys ) );
  
(* Takes a list of Bindings and returns true if the Bindings are consistent, 
   along with the expanded list of Bindings which includes those Bindings which 
   are necessary to make the input Bindings unify. *)
fun consistentBindings( [] ) = ( true, [] )
  | consistentBindings( bindings ) = 
    let fun worker( [], results ) = ( true, results )
          | worker( (binding::bindings), results ) = 
            let val x as (success, result) = unify( Unifier([]), binding )
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
and unify( Unifier(defaultBindings),
            Binding( Term( funcA, argsA ), Term( funcB, argsB ) ) ) = 
    (* check that the functors and arities match *)
    let val successOfThis = 
                ( funcA = funcB ) andalso 
                ( length( argsA ) = length( argsB ) )
    in
        if( not successOfThis ) then ( false, Unifier([]) ) else
    (* unify the arguments pairwise *)
    let fun unifyAll( _, [] ) = []
          | unifyAll( unifier, binding::bindings ) = 
                unify( unifier, binding ) :: ( unifyAll( unifier, bindings ) )
        val unificationOfArgs = 
                unifyAll( Unifier([]), zipBinding( argsA, argsB ) )
    (* check that all the arguments unified successfully *)
        val successOfArgs = andList ( map first unificationOfArgs )
    in
        if( not( successOfArgs ) ) then ( false, Unifier([]) ) else 
    (* verify that the resulting unifier is consistent *)
    let val rawBindings = combineUnifiers( 
            ( Unifier(defaultBindings) )::( map second unificationOfArgs ) )
        val finalUnifier = validateUnifier( rawBindings )
    in
        if ( first( finalUnifier ) )
            then ( true, ( second( finalUnifier ) ) )
        else
            ( false, Unifier([]) )
    end end end
    
  | unify( Unifier(xs), Binding( IntTerm(i1), IntTerm(i2) ) ) =
        if( i1 = i2 )
            then ( true, Unifier(xs) )
        else
            ( false, Unifier([]) )
  | unify( Unifier(xs), Binding( FloatTerm(f1), FloatTerm(f2) ) ) =
        if( Real.==( f1, f2 ) )
            then ( true, Unifier(xs) )
        else
            ( false, Unifier([]) )

  (* Reject incompatible cross-combinations of terms. *)
  | unify( _, Binding( Term(_), IntTerm(_) ) ) = ( false, Unifier([]) )
  | unify( _, Binding( IntTerm(_), Term(_) ) ) = ( false, Unifier([]) )
  | unify( _, Binding( Term(_), FloatTerm(_) ) ) = ( false, Unifier([]) )
  | unify( _, Binding( FloatTerm(_), Term(_) ) ) = ( false, Unifier([]) )
  | unify( _, Binding( IntTerm(_), FloatTerm(_) ) ) = ( false, Unifier([]) )
  | unify( _, Binding( FloatTerm(_), IntTerm(_) ) ) = ( false, Unifier([]) )
  (* Remaining cases must involve a variable. *)
  | unify( Unifier([]), Binding( x, y ) ) = 
        ( true, Unifier( [ Binding( x,y ) ] ) )
  | unify( Unifier( defaultBindings ), Binding( x, y ) ) = 
        validateUnifier( ( Binding( x, y ) ):: defaultBindings );

