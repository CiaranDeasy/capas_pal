val scopeCounter = ref 2;

(* Takes two lists of equal length, and returns a list of 2-tuples, where the 
   i'th tuple contains the i'th element of each input list. *)
fun zipBinding( [], [] ) = []
  | zipBinding( (x::xs), (y::ys) ) = Binding(x,y)::( zipBinding( xs, ys ) );
  
(* Takes two Bindings (A,B) and (C,D) and returns a (bool, Binding) tuple. If 
   B=C or B=D, then the first value is true, and the second value is (A,D) or 
   (A,C) respectively. Binding containing the two distinct terms (ie: the 
   Binding that exists by transitivity). If they do not share a common term, or 
   if the Bindings are equal, then the first value is false. *)
fun getLeftTransitiveBinding( Binding( leftTerm, rightTerm ), 
            Binding( term2A, term2B ) ) = 
        if( eqBinding( ( Binding( leftTerm, rightTerm ) ), 
                ( Binding( term2A, term2B ) ) ) )
            then ( false, Binding( leftTerm, rightTerm ) )
        else if( eqTerm( rightTerm, term2A ) )
            then ( true, Binding( leftTerm, term2B ) )
        else if( eqTerm( rightTerm, term2B ) )
            then ( true, Binding( leftTerm, term2A ) )
        else ( false, Binding( leftTerm, rightTerm ) );
        
fun getRightTransitiveBinding( Binding( leftTerm, rightTerm ), 
            Binding( term2A, term2B ) ) = 
        if( eqBinding( ( Binding( leftTerm, rightTerm ) ), 
                ( Binding( term2A, term2B ) ) ) )
            then ( false, Binding( leftTerm, rightTerm ) )
        else if( eqTerm( leftTerm, term2A ) )
            then ( true, Binding( term2B, rightTerm ) )
        else if( eqTerm( leftTerm, term2B ) )
            then ( true, Binding( term2A, rightTerm ) )
        else ( false, Binding( leftTerm, rightTerm ) );
        
(* Takes a binding x and a list of bindings. Returns all transitive bindings 
   that are implied by adding x to the list. *)
(*fun getAllTransitiveBindings( x, [] ) = []
  | getAllTransitiveBindings( x, (y::ys) ) = 
    let val transitive = getTransitiveBinding( x, y ) in
        if( first( transitive ) ) then 
            ( second( transitive ) ) :: ( getAllTransitiveBindings( x, ys ) )
        else
            getAllTransitiveBindings( x, ys )
    end;*)
    
fun getAllBindingPairs( [], _ ) = []
  | getAllBindingPairs( Binding( _, term1 )::lefts, rights ) =
    let fun worker( [] ) = []
          | worker( Binding( term2, _ )::rights ) = 
                Binding( term2, term1 ) :: worker( rights )
    in
        worker( rights ) @ ( getAllBindingPairs( lefts, rights ) )
    end;
    
fun getAllTransitiveBindings( newBinding, bindings ) = 
    let fun worker( _, [], lefts, rights ) = 
                lefts @ rights @ ( getAllBindingPairs( lefts, rights ) )
          | worker( newBinding, binding::bindings, lefts, rights ) = 
            let val x as (leftSucc, leftTransBinding) = 
                        getLeftTransitiveBinding( newBinding, binding )
                val x as (rightSucc, rightTransBinding) = 
                        getRightTransitiveBinding( newBinding, binding )
            in
                if( leftSucc ) then 
                    worker( newBinding, bindings, 
                            leftTransBinding::lefts, rights )
                else if( rightSucc ) then 
                    worker( newBinding, bindings, 
                            lefts, rightTransBinding::rights )
                else worker( newBinding, bindings, lefts, rights )
            end
    in
        worker( newBinding, bindings, [], [] )
    end;
    
(* Takes a list of Unifiers and returns a list of all the Bindings that occur 
   in them, potentially with duplicates. *)
fun combineUnifiers( [] ) = []
  | combineUnifiers( ( Unifier(xs) )::ys ) = xs @ ( combineUnifiers( ys ) );
  
(* Takes a list of Bindings and returns a two-tuple. The first value is a bool 
   indicating whether the bindings are consistent, and the second value is a 
   list of Bindings that must hold for the input list to hold. *)
fun consistentBindings( bindings ) = 
    let fun worker( [], newBindings ) = ( true, newBindings )
          | worker( binding::bindings, prevNewBindings ) =
            let val x as (succ, Unifier(xs)) = unify( Unifier([]), binding )
                val nextNewBindings = polyRemove eqBinding ( binding, xs )
            in
                if( not(succ) ) then
                    ( false, [] )
                else
                    worker( bindings, ( nextNewBindings @ prevNewBindings ) )
            end
    in
        worker( bindings, [] )
    end
  
(* Takes a list of bindings and returns a (bool, Unifier) tuple. If the bindings
   are consistent, then the first value is true and the second value is a 
   Unifier containing the bindings. If the bindings are inconsistent, then the 
   first value is false. *)
and validateUnifier( currentBindings, newBinding ) = 
    (* Get the transitive bindings to which this new binding gives rise. *)
    let val transitiveBindings = 
                getAllTransitiveBindings( newBinding, currentBindings )
    (* Test whether the bindings are consistent. *)
         val x as (success, iteratedBindings) = 
            consistentBindings( transitiveBindings )
    in
        if( success ) then
            (* Verify any new Bindings that arose when verifying the transitive
               bindings *)
            unifyAll( Unifier( 
                    newBinding::(transitiveBindings @ currentBindings) 
                    ), iteratedBindings )
        else
            ( false, Unifier( [] ) )
    end

(* Takes a unifier and a list of bindings. Successively calls unify() on each 
   binding, using the unifier from the previous unification. Returns the total 
   unification result. *)
and unifyAll( unifier, [] ) = ( true, unifier )
  | unifyAll( unifier, binding::bindings ) = 
    let val x as (succ, newUnifier) = unify( unifier, binding )
    in
        if( succ ) then
            unifyAll( newUnifier, bindings )
        else
            ( false, Unifier([]) )
    end

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
            unifyAll( Unifier(defaultBindings), zipBinding( argsA, argsB ) )
    end
    
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
  | unify( Unifier( defaultBindings ), newBinding ) = 
        if( memberPoly( newBinding, defaultBindings, eqBinding ) ) then
            ( true, Unifier( defaultBindings ) )
        else
            validateUnifier( defaultBindings, newBinding );

