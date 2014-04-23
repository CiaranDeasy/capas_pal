(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the unify() function used to perform unification in 
the Prolog interpreter/compiler, and a set of helper functions used in 
unification.
*******************************************************************************)

val scopeCounter = ref 2;
fun getScope() = (
        scopeCounter := !scopeCounter + 1;
        !scopeCounter
)

(* Takes two lists of equal length, and returns a list of 2-tuples, where the 
   i'th tuple contains the i'th element of each input list. *)
fun zipBinding( [], [] ) = []
  | zipBinding( (x::xs), (y::ys) ) = Binding(x,y)::( zipBinding( xs, ys ) );
  
(* If both terms are variables, returns true if they are equal, false otherwise. 
   If either term is a non-variable, returns false. *)
fun eqVariable( Variable( v1, s1 ), Variable( v2, s2 ) ) = 
        eqTerm( Variable( v1, s1 ), Variable( v2, s2 ) )
  | eqVariable( _, _ ) = false;
  
(* Takes two Bindings (A,B) and (C,D) and returns a (bool, Binding) tuple. If 
   B is a Variable and B=C or B=D, then the first value is true, and the second 
   value is (A,D) or (A,C) respectively, ie: the Binding that exists by 
   transitivity. If they do not share a common term, or if the Bindings are 
   equal, then the first value is false. *)
fun getLeftTransitiveBinding( Binding( leftTerm, rightTerm ), 
            Binding( term2A, term2B ) ) = 
        if( eqBinding( ( Binding( leftTerm, rightTerm ) ), 
                ( Binding( term2A, term2B ) ) ) )
            then ( false, Binding( leftTerm, rightTerm ) )
        else if( eqVariable( rightTerm, term2A ) )
            then ( true, Binding( leftTerm, term2B ) )
        else if( eqVariable( rightTerm, term2B ) )
            then ( true, Binding( leftTerm, term2A ) )
        else ( false, Binding( leftTerm, rightTerm ) );
        
(* Takes two Bindings (A,B) and (C,D) and returns a (bool, Binding) tuple. If 
   A is a Variable and A=C or A=D, then the first value is true, and the second 
   value is (B,D) or (B,C) respectively, ie: the Binding that exists by 
   transitivity. If they do not share a common term, or if the Bindings are 
   equal, then the first value is false. *)
fun getRightTransitiveBinding( Binding( leftTerm, rightTerm ), 
            Binding( term2A, term2B ) ) = 
        if( eqBinding( ( Binding( leftTerm, rightTerm ) ), 
                ( Binding( term2A, term2B ) ) ) )
            then ( false, Binding( leftTerm, rightTerm ) )
        else if( eqVariable( leftTerm, term2A ) )
            then ( true, Binding( term2B, rightTerm ) )
        else if( eqVariable( leftTerm, term2B ) )
            then ( true, Binding( term2A, rightTerm ) )
        else ( false, Binding( leftTerm, rightTerm ) );
    
(* Takes two lists of bindings. Returns a list of bindings which bind the right 
   terms from bindings in the first list with the left terms from bindings in 
   the second list. *)
fun getAllBindingPairs( [], _ ) = []
  | getAllBindingPairs( Binding( _, term1 )::lefts, rights ) =
    let fun worker( [] ) = []
          | worker( Binding( term2, _ )::rights ) = 
                Binding( term2, term1 ) :: worker( rights )
    in
        worker( rights ) @ ( getAllBindingPairs( lefts, rights ) )
    end;
    
(* Takes a new binding and a list of existing bindings. Returns a list of all of
   the bindings implied transitively by adding the new binding to the list of 
   existing bindings. The new binding is not included in the output list. *)
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
    
(* Takes a list of bindings, removes bindings which don't contain a variable, 
   and returns a list of the remaining bindings. *)
fun removeTermToTermBindings( [] ) = []
  | removeTermToTermBindings( Binding( Variable( v, s ), t2 )::bindings ) = 
        Binding( Variable( v, s ), t2 )::removeTermToTermBindings( bindings )
  | removeTermToTermBindings( Binding( t1, Variable( v, s ) )::bindings ) = 
        Binding( t1, Variable( v, s ) )::removeTermToTermBindings( bindings )
  | removeTermToTermBindings( binding::bindings ) = 
        removeTermToTermBindings( bindings );
    
(* Takes a list of Unifiers and returns a list of all the Bindings that occur 
   in them, potentially with duplicates. *)
fun combineUnifiers( [] ) = []
  | combineUnifiers( ( Unifier(xs) )::ys ) = xs @ ( combineUnifiers( ys ) );
  
exception derpl;
  
(* Takes a single binding and a list of bindings. If the single binding binds a 
   variable to a term, and a binding in the list binds the same variable to a 
   term, then returns true. Otherwise false. *)
    (* Dismiss V-V main binding *)
fun alreadyBound( Binding( Variable( _, _ ), Variable( _, _ ) ), _ ) = false
    (* Base case *)
  | alreadyBound( _, [] ) = false
    (* Flip backwards main binding *)
  | alreadyBound( Binding( t, Variable( v, s ) ), bindings ) = 
        alreadyBound( Binding( Variable( v, s ), t ), bindings )
    (* Ignore V-V list binding *)
  | alreadyBound( binding, 
                  Binding( Variable( _, _ ), Variable( _, _ ) )::bindings ) =
        alreadyBound( binding, bindings )
    (* Flip backwards list binding *)
  | alreadyBound( binding, Binding( t, Variable( v, s ) )::bindings ) =
        alreadyBound( binding, Binding( Variable( v, s ), t )::bindings )
    (* Main case: compare T-V with T-V *)
  | alreadyBound( Binding( Variable( v1, s1 ), t1 ), 
                  Binding( Variable( v2, s2 ), _ )::bindings ) = 
        if( eqTerm( Variable( v1, s1 ), Variable( v2, s2 ) ) ) then
            true
        else
            alreadyBound( Binding( Variable( v1, s1 ), t1 ), bindings )
            
fun isVarToVarBinding( Binding( Variable( _, _ ), Variable( _, _ ) ) ) = true
  | isVarToVarBinding( _ ) = false;
        
(* Takes a list of new bindings and a list of current bindings. Removes from new
   bindings any V-T binding where the variable is already bound in current 
   bindings. Returns the remaining new bindings. *)
fun removeAlreadyBoundVariables( newBindings, currentBindings ) = 
          (* Base case. *)
    let fun worker( [] ) = []
          (* Skip V-V Bindings. *)
          | worker( Binding( Variable( v1, s1 ), Variable( v2, s2 ) )::
                    bindings ) =
                Binding( Variable( v1, s1 ), Variable( v2, s2 ) )::
                        worker( bindings )
          (* Remove V-T bindings if the variable is already bound. *)
          | worker( Binding( Variable( v1, s1 ), t2 )::bindings ) = 
            let val binding = Binding( Variable( v1, s1 ), t2 )
            in
                if( alreadyBound( binding, currentBindings ) ) then
                    worker( bindings )
                else
                    binding::worker( bindings )
            end
          (* Flip T-V bindings into V-T bindings *)
          | worker( Binding( t1, Variable( v2, s2 ) )::bindings ) =
                worker( Binding( Variable( v2, s2 ), t1 )::bindings )
          (* T-T bindings are an error, so don't match that pattern. *)
    in
        worker( newBindings )
    end;
  
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
        val sanitisedTransitiveBindings = 
                removeTermToTermBindings( transitiveBindings )
    in
        if( not( success ) ) then
            ( false, Unifier( [] ) )
        else if( alreadyBound( newBinding, currentBindings ) ) then
            (* Don't include the newBinding in the Unifier *)
            unifyAll( Unifier( 
                    (sanitisedTransitiveBindings @ currentBindings) 
                    ), iteratedBindings )
        else if( isVarToVarBinding( newBinding ) ) then
            (* This can produce duplicate bindings, so we have to manually check
               that every V-T transitive binding is unbound *)
            let val finalTransitiveBindings = removeAlreadyBoundVariables( 
                        sanitisedTransitiveBindings, currentBindings )
            in
                unifyAll( Unifier( 
                        newBinding::(finalTransitiveBindings @ currentBindings) 
                        ), iteratedBindings )
            end
        else
            (* Verify any new Bindings that arose when verifying the transitive
               bindings *)
            unifyAll( Unifier( 
                    newBinding::(sanitisedTransitiveBindings @ currentBindings) 
                    ), iteratedBindings )
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

