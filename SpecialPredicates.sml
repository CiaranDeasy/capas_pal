exception UninstantiatedVariable;
    
(* Takes a variable and a unifier. Scans the unifier and returns the term to 
   which the variable is bound. Raises an exception if no binding is found. *)
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
    
fun atomic_1( unifier, Term( _, [] ), k1, k2 ) = 
        k1 unifier k2
  | atomic_1( unifier, IntTerm(_), k1, k2 ) = 
        k1 unifier k2
  | atomic_1( unifier, FloatTerm(_), k1, k2 ) = 
        k1 unifier k2
  | atomic_1( unifier, Variable( v,s ), k1, k2 ) = 
        let val result = ( atomic_1( 
          unifier, ( subVarForTerm( Variable( v, s ), unifier ) ), k1, k2 ) )
          handle UninstantiatedVariable => ( k2() ) 
        in 
            result
        end
  | atomic_1( _, _, _, k2 ) = k2();
    
fun is_2( unifier, arg1, arg2, k1, k2 ) = 
    let val evaluation = evaluate( arg2, unifier ) in
    let val x as (success, newUnifier) = 
            unify unifier ( Binding( arg1, evaluation ) )
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
          
    
