(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file defines a number of special-purpose built-in predicates which 
are available to all Prolog programs.
*******************************************************************************)

exception UninstantiatedVariable;
exception EvaluationTypeError;
    
(* Negates the value of an IntTerm or a FloatTerm. *)
fun negate( IntTerm(i) ) = IntTerm(~i)
  | negate( FloatTerm(f) ) = FloatTerm(~f)
  | negate( Term( Functor(f), [arg] ) ) = 
        if( f = "-" ) then
            arg
        else
            raise EvaluationTypeError;
  
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
    (* Returns positive IntTerms and FloatTerms as they are. Returns negative 
       IntTerms and FloatTerms as positive terms wrapped in a unary minus Term.
       Terms which already have a unary minus are also returned as they are. *)
    let fun formatNegation( IntTerm(i) ) = 
                if( i >= 0 ) then 
                    IntTerm(i) 
                else
                    Term( Functor("-"), [ IntTerm(~i) ] )
          | formatNegation( FloatTerm(f) ) = 
                if( f >= 0.0 ) then 
                    FloatTerm(f) 
                else
                    Term( Functor("-"), [ FloatTerm(~f) ] )
          | formatNegation( Term( Functor(f), [arg] ) ) = 
                Term( Functor(f), [arg] )
        fun addTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 + i2 )
          | addTerms( IntTerm(i1), FloatTerm(f2) ) = FloatTerm( real(i1) + f2 )
          | addTerms( FloatTerm(f1), IntTerm(i2) ) = FloatTerm( f1 + real(i2) )
          | addTerms( FloatTerm(f1), FloatTerm(f2) ) = FloatTerm( f1 + f2 )
          | addTerms( Term( Functor(func1), [arg1] ), term ) =
                if( func1 = "-" ) then
                    formatNegation( addTerms( 
                            negate( evaluate( arg1, unifier ) ), term ) )
                else
                    raise EvaluationTypeError
          | addTerms( term, Term( Functor(func2), [arg2] ) ) =
                addTerms( Term( Functor(func2), [arg2] ), term )
          | addTerms( term1, term2 ) = raise EvaluationTypeError
    in
    let fun subTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 - i2 )
          | subTerms( IntTerm(i1), FloatTerm(f2) ) = FloatTerm( real(i1) - f2 )
          | subTerms( FloatTerm(f1), IntTerm(i2) ) = FloatTerm( f1 - real(i2) )
          | subTerms( FloatTerm(f1), FloatTerm(f2) ) = FloatTerm( f1 - f2 )
          | subTerms( Term( Functor(func1), [arg1] ), term ) =
                if( func1 = "-" ) then
                    formatNegation( subTerms( 
                            negate( evaluate( arg1, unifier ) ), term ) )
                else
                    raise EvaluationTypeError
          | subTerms( term, Term( Functor(func2), [arg2] ) ) =
                if( func2 = "-" ) then
                    formatNegation( subTerms( 
                            term, negate( evaluate( arg2, unifier ) ) ) )
                else
                    raise EvaluationTypeError
          | subTerms( term1, term2 ) = raise EvaluationTypeError
    in
    let fun mulTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 * i2 )
          | mulTerms( IntTerm(i1), FloatTerm(f2) ) = FloatTerm( real(i1) * f2 )
          | mulTerms( FloatTerm(f1), IntTerm(i2) ) = FloatTerm( f1 * real(i2) )
          | mulTerms( FloatTerm(f1), FloatTerm(f2) ) = FloatTerm( f1 * f2 )
          | mulTerms( Term( Functor(func1), [arg1] ), term ) =
                if( func1 = "-" ) then
                    formatNegation( mulTerms( 
                            negate( evaluate( arg1, unifier ) ), term ) )
                else
                    raise EvaluationTypeError
          | mulTerms( term, Term( Functor(func2), [arg2] ) ) =
                mulTerms( Term( Functor(func2), [arg2] ), term )
          | mulTerms( term1, term2 ) = raise EvaluationTypeError
    in
    let fun divTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 div i2 )
          | divTerms( IntTerm(i1), FloatTerm(f2) ) = FloatTerm( real(i1) / f2 )
          | divTerms( FloatTerm(f1), IntTerm(i2) ) = FloatTerm( f1 / real(i2) )
          | divTerms( FloatTerm(f1), FloatTerm(f2) ) = FloatTerm( f1 / f2 )
          | divTerms( Term( Functor(func1), [arg1] ), term ) =
                if( func1 = "-" ) then
                    formatNegation( divTerms( 
                            negate( evaluate( arg1, unifier ) ), term ) )
                else
                    raise EvaluationTypeError
          | divTerms( term, Term( Functor(func2), [arg2] ) ) =
                if( func2 = "-" ) then
                    formatNegation( divTerms( 
                            term, negate( evaluate( arg2, unifier ) ) ) )
                else
                    raise EvaluationTypeError
          | divTerms( term1, term2 ) = raise EvaluationTypeError
    in
    let fun modTerms( IntTerm(i1), IntTerm(i2) ) = IntTerm( i1 mod i2 )
          | modTerms( Term( Functor(func1), [arg1] ), term ) =
                if( func1 = "-" ) then
                    formatNegation( modTerms( 
                            negate( evaluate( arg1, unifier ) ), term ) )
                else
                    raise EvaluationTypeError
          | modTerms( term, Term( Functor(func2), [arg2] ) ) =
                if( func2 = "-" ) then
                    formatNegation( modTerms( 
                            term, negate( evaluate( arg2, unifier ) ) ) )
                else
                    raise EvaluationTypeError
          | modTerms( term1, term2 ) = raise EvaluationTypeError
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
    
fun atomic_1( unifier, Term( _, [] ), k1, k2 ) = k1( unifier, k2 )
  | atomic_1( unifier, IntTerm(_), k1, k2 ) = k1( unifier, k2 )
  | atomic_1( unifier, FloatTerm(_), k1, k2 ) = k1( unifier, k2 )
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
            unify( unifier, Binding( arg1, evaluation ) )
    in
        if( success ) then
            k1( newUnifier, k2 )
        else
            k2()
    end end;
    
fun specialPredicateLess( unifier, IntTerm(i1), IntTerm(i2), k1, k2 ) =
        if( i1 < i2 ) then
            k1( unifier, k2 )
        else
            k2()
  | specialPredicateLess( unifier, IntTerm(i1), FloatTerm(f2), k1, k2 ) =
        if( real(i1) < f2 ) then
            k1( unifier, k2 )
        else
            k2()
  | specialPredicateLess( unifier, FloatTerm(f1), IntTerm(i2), k1, k2 ) =
        if( f1 < real(i2) ) then
            k1( unifier, k2 )
        else
            k2()
  | specialPredicateLess( unifier, FloatTerm(f1), FloatTerm(f2), k1, k2 ) =
        if( f1 < f2 ) then
            k1( unifier, k2 )
        else
            k2()
  | specialPredicateLess( 
            unifier, Term( Functor(func1), [arg1] ), term2, k1, k2 ) =
        if( func1 = "-" ) then
            specialPredicateLess( unifier, negate( arg1 ), term2, k1, k2 )
        else
            raise EvaluationTypeError
  | specialPredicateLess( 
            unifier, term1, Term( Functor(func2), [arg2] ), k1, k2 ) =
        if( func2 = "-" ) then
            specialPredicateLess( unifier, term1, negate( arg2 ), k1, k2 )
        else
            raise EvaluationTypeError
  | specialPredicateLess( unifier, Variable( v, s ), arg2, k1, k2 ) =
        specialPredicateLess( unifier, 
                subVarForTerm( Variable( v, s ), unifier ), arg2, k1, k2 )
  | specialPredicateLess( unifier, arg1, Variable( v, s ), k1, k2 ) =
        specialPredicateLess( unifier, arg1, 
                subVarForTerm( Variable( v, s ), unifier ), k1, k2 );
          
fun specialPredicateGreater( unifier, term1, term2, k1, k2 ) =
        specialPredicateLess( unifier, term2, term1, k1, k2 );

fun specialPredicatePrint1( unifier, arg1, k1, k2 ) = 
        ( printTerm( arg1 ); k1( unifier, k2 ) );

fun specialPredicateEquals( unifier, arg1, arg2, k1, k2 ) = 
    let val x as (succ, newUnifier ) = unify( unifier, Binding( arg1, arg2 ) )
    in
        if( succ ) then
            k1( newUnifier, k2 )
        else
            k2()
    end

fun specialPredicateCut( unifier, succ, globalFail ) = 
        succ( unifier, globalFail );

fun specialPredicateFail( unifier, succ, fail ) = fail();

(*fun specialPredicateListing( unifier, arg, succ, fail ) = *)

(*fun specialPredicateDelete( unifier, arg1, arg2, arg3, succ, fail ) = 
        if( eqTerm( arg1, Term( Functor( "[]" ), [] ) ) ) then
            succ( unifier, fail )
        else
            let val x as Term( Functor(f), [head,tail] ) = arg2
                val x as (matches, _) = unify( arg2, head )
            in
                if( matches ) then
                    let fun newSucc( newUni, _ ) = 
                    let val x as (tailSucc, tailUnifier) = 
                    specialPredicateDelete( *)
                    
(*fun specialPredicateUnifyWithoutBinding( unifier, arg1, arg2, succ, fail ) = 
    let val x as (success, _) = *)