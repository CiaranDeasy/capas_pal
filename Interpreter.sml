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
            let val x as [arg1, arg2] = args in
                specialPredicateGreater( unifier, arg1, arg2, k1, k3 )
            end
        else if( ( f = "<" ) andalso ( List.length( args ) = 2 ) ) then
            let val x as [arg1, arg2] = args in
                specialPredicateLess( unifier, arg1, arg2, k1, k3 )
            end
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
