(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the functions for interpreting a Prolog program and 
finding a Unifier, if any, that satisfies a query.
*******************************************************************************)

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

(* Takes a term, a unifier and three continuations. If the term is one of the 
   special built-in predicates, then it is evaluated by a special-purpose 
   function. If it succeeds, k1 is called with the new unifier and with k3 as a 
   backtracking continuation. If it fails, then the backtracking continuation k3
   is called. If the term is not a special predicate, then k2 is called to 
   evaluate it the normal way. *)
fun specialPredicate( IntTerm(i), _, _, k2, _ ) = k2()
  | specialPredicate( FloatTerm(i), _, _, k2, _ ) = k2()
  | specialPredicate( Variable(i), _, _, k2, _ ) = k2()
  | specialPredicate( Term( Functor( f ), args ), unifier, k1, k2, k3 ) = 
        if( ( f = "atomic" ) andalso ( List.length( args ) = 1 ) ) then
            let val x as [arg1] = args in
                atomic_1( unifier, arg1, k1, k3 )
            end
        else if( ( f = "is" ) andalso ( List.length( args ) = 2 ) ) then
            let val x as [arg1, arg2] = args in
                is_2( unifier, arg1, arg2, k1, k3 )
            end
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
fun findUnifier( program, term, unifier, k1, k2 ) = 
    (* Takes a list of terms and finds a unifier that satisfies all of them. *)
    let fun findUnifiers( [], unifier, k1, k2 ) = k1( unifier, k2 )
          | findUnifiers( (term::terms), unifier, k1, k2 ) = 
            let fun m1( newUnifier, k3 ) = 
                    findUnifiers( terms, newUnifier, k1, k3 )
            in 
                findUnifier( program, term, unifier, m1, k2 )
            end in
    let fun worker( [] ) = k2()
          | worker( ( clause as Clause( _, _ ) )::clauses ) = 
            let val scope = !scopeCounter in 
            let val liveClause as Clause( head, body ) = 
                        scopeClause( clause, scope ) in 
            let val incScope = ( scopeCounter := !scopeCounter + 1 ) in
            let val unification = unify( unifier, ( Binding( term, head ) ) )
            in
                if ( first( unification ) ) then 
                    let fun m2() = worker( clauses )
                        in 
                            findUnifiers( 
                                    body, ( second ( unification ) ), k1, m2 )
                        end
                else
                    worker( clauses )
            end end end end in
    let fun getClauses( Program(xs) ) = xs in
    (* Make a continuation for running through the clauses. *)
    let fun m1() = worker( getClauses( program ) )
    in
        (* But first check if we're dealing with a special predicate. *)
        specialPredicate( term, unifier, k1, m1, k2 )
    end end end end;
    
(* Takes a program, a query and two continuations. If the query is satisfiable 
   by the program, then the first continuation is called with the most general 
   unifier that satisfies it. If not, then the second continuation is called 
   with unit. *)
fun executeQuery( program, (Query(xs)), k1, k2 ) = 
    let fun executeQueryTerms( [], unifier, k1, _ ) = k1( unifier )
          | executeQueryTerms( (x::xs), unifier, k1, k2 ) = 
            (* Executes the next term. *)
            let fun m1( newUnifier, k ) = 
                        executeQueryTerms( xs, newUnifier, k1, k )
            in
                findUnifier( program, x, unifier, m1, k2 )
            end
    in
        executeQueryTerms( xs, (Unifier([])), k1, k2 )
    end;
    
(* Takes two term-to-variable bindings, and applies the first to the second as a
   substitution, returning the updated second binding. Returns the binding 
   unchanged if either binding is variable-to-variable. *)
(* If either Binding is purely variables, then there is no change. *)
fun substitute( Binding( Variable( _, _ ), Variable( _, _ ) ), b2 ) = b2
  | substitute( b1, Binding( Variable( v1, s1 ), Variable( v2, s2 ) ) ) = 
        Binding( Variable( v1, s1 ), Variable( v2, s2 ) )
(* Mirror bindings if the variable comes before the term. *)
  | substitute( Binding( Variable( v1, s1 ), t1 ), b2 ) = 
        substitute( Binding( t1, Variable( v1, s1 ) ), b2 )
  | substitute( b1, Binding( Variable( v2, s2 ), t2 ) ) = 
        substitute( b1, Binding( t2, Variable( v2, s2 ) ) )
(* No substitution if the target is an IntTerm or a FloatTerm binding *)
  | substitute( b1, Binding( IntTerm(i), Variable(v,s) ) ) = 
        Binding( IntTerm(i), Variable(v,s) )
  | substitute( b1, Binding( FloatTerm(f), Variable(v,s) ) ) = 
        Binding( FloatTerm(f), Variable(v,s) )
(* Main version when the target is a Term: *)
  | substitute( Binding( term, Variable( v1, s1 ) ),
            Binding( Term( f2, args2 ), Variable( v2, s2 ) ) ) =
    let fun worker( Term( f, args ) ) = 
            let fun iterateArgs( [] ) = []
                  | iterateArgs( arg::remaining ) = 
                        ( worker( arg ) ) :: ( iterateArgs( remaining ) )
            in
                Term( f, ( iterateArgs( args ) ) )
            end
          | worker( Variable( v, s ) ) = 
                if( eqTerm( Variable( v, s ), Variable( v1, s1 ) ) )
                    then term
                else Variable( v, s )
          | worker( IntTerm(i) ) = IntTerm(i)
          | worker( FloatTerm(f) ) = FloatTerm(f)
    in
        Binding( ( worker( Term( f2, args2 ) ) ), Variable( v2, s2 ) )
    end;

(* Takes a consistent Unifier with no term-to-term Bindings. Expands Bindings 
   containing Terms by replacing Variables with Terms according to other 
   Bindings in the Unifier. *)
fun substituteUnifier( Unifier(xs) ) = 
    let fun addBinding( Binding( x, y ), bindings ) = 
        let fun firstRound( newBinding, [] ) = newBinding
              | firstRound( newBinding, ( nextBinding::remaining ) ) = 
                    firstRound( substitute(nextBinding, newBinding), remaining )
        in
        let fun secondRound( newBinding, [] ) = [ newBinding ]
              | secondRound( newBinding, ( nextBinding::remaining ) ) = 
                    ( substitute( newBinding, nextBinding ) )::
                            ( secondRound( newBinding, remaining ) )
        in
            secondRound( firstRound( Binding( x, y ), bindings ), bindings )
        end end in
    let fun worker( [], ys ) = Unifier(ys)
          | worker( (x::xs), ys ) = worker( xs, ( addBinding( x, ys ) ) )
    in
        worker( xs, [] )
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
    let fun m1( unifier ) = ( 
            printQuery( x ); 
            print "\n";
            (* Remove bindings which don't feature a variable. *)
            let val reducedUnifier = cleanUnifier( unifier ) in
            (* Perform variable substitutions. *)
            let val subUnifier = substituteUnifier( reducedUnifier ) in
                (* Print the Unifier. *)
                printUnifier( subUnifier ) 
            end end;
            executeQueries( program, xs) 
        )
        fun m2() = ( 
            printQuery( x ); 
            print "\nQuery not satisfiable.\n"; 
            false 
        ) 
    in
        executeQuery( program, x, m1, m2 )
    end;
