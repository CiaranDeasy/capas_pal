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

(* Takes a term, a unifier and four continuations. If the term is one of the 
   special built-in predicates, then it is evaluated by a special-purpose 
   function. If it succeeds, the success continuation is called with the new 
   unifier and with the localFail continuation as a backtracking continuation. 
   If it fails, then the backtracking continuation localFail is called. If the 
   term is not a special predicate, then the notSpecial continuation is called 
   to evaluate it the normal way. The globalFail continuation is used by the 
   cut operator. *)
fun specialPredicate( IntTerm(i), _, _, notSpecial, _, _ ) = notSpecial()
  | specialPredicate( FloatTerm(i), _, _, notSpecial, _, _ ) = notSpecial()
  | specialPredicate( Variable(i), _, _, notSpecial, _, _) = notSpecial()
  | specialPredicate( Term( Functor( f ), args ), 
            unifier, succ, notSpecial, localFail, globalFail ) = 
        if( ( f = "atomic" ) andalso ( List.length( args ) = 1 ) ) then
            let val x as [arg1] = args in
                atomic_1( unifier, arg1, succ, localFail )
            end
        else if( ( f = "is" ) andalso ( List.length( args ) = 2 ) ) then
            let val x as [arg1, arg2] = args in
                is_2( unifier, arg1, arg2, succ, localFail )
            end
        else if( ( f = ">" ) andalso ( List.length( args ) = 2 ) ) then
            let val x as [arg1, arg2] = args in
                specialPredicateGreater( unifier, arg1, arg2, succ, localFail )
            end
        else if( ( f = "<" ) andalso ( List.length( args ) = 2 ) ) then
            let val x as [arg1, arg2] = args in
                specialPredicateLess( unifier, arg1, arg2, succ, localFail )
            end
        else if( ( f = "print" ) andalso ( List.length( args ) = 1 ) ) then
            let val x as [arg1] = args in
                specialPredicatePrint1( unifier, arg1, succ, localFail )
            end
        else if( ( f = "=" ) andalso ( List.length( args ) = 2 ) ) then
            let val x as [arg1, arg2] = args in
                specialPredicateEquals( unifier, arg1, arg2, succ, localFail )
            end
        else if( ( f = "!" ) andalso ( List.length( args ) = 0 ) ) then
                 specialPredicateCut( unifier, succ, globalFail )
        else if( ( f = "fail" ) andalso ( List.length( args ) = 0 ) ) then
                 specialPredicateFail( unifier, succ, localFail )
        else if( ( f = "delete" ) andalso ( List.length( args ) = 3 ) ) then
            let val x as [arg1, arg2, arg3] = args in
                delete_3( unifier, arg1, arg2, arg3, succ, localFail )
            end
        else
            notSpecial();
        
(* Takes a term, an input Unifier and two continuations. If the term is 
   satisfiable by the (hardcoded) Prolog program, in a way that is consistent 
   with the input Unifier, then the first continuation is called with the 
   updated Unifier. If not, then the second continuation is called with unit. *)
fun findUnifier( program, term, unifier, succ, localFail, globalFail ) = 
    (* Takes a list of terms and finds a unifier that satisfies all of them. *)
    let fun findUnifiers( terms, unifier, scope, globalSucc, localFail, globalFail ) = 
            let fun worker( [], unifier, succ, localFail ) =
                        succ( cleanupScope( unifier, scope ), localFail )
                  | worker( (term::terms), unifier, globalSucc, localFail ) = 
                    let fun localSucc( newUnifier, newFail ) = 
                                worker( terms, newUnifier, globalSucc, newFail )
                    in 
                        findUnifier( program, term, unifier, localSucc, 
                                localFail, globalFail )
                    end 
            in
                worker( terms, unifier, globalSucc, localFail )
            end
        fun worker( [] ) = localFail()
          | worker( ( clause as Clause( _, _ ) )::clauses ) = 
            let val scope = getScope()
                val liveClause as Clause( head, body ) = 
                        scopeClause( clause, scope ) 
                val unification = unify( unifier, ( Binding( term, head ) ) )
            in
                if ( first( unification ) ) then 
                    let fun newLocalFail() = worker( clauses )
                        in 
                            findUnifiers( 
                                    body, ( second ( unification ) ), scope, 
                                      succ, newLocalFail, localFail )
                        end
                else
                    worker( clauses )
            end 
        fun getClauses( Program(xs) ) = xs
    (* Make a continuation for running through the clauses. *)
        fun notSpecial() = worker( getClauses( program ) )
    in
        (* But first check if we're dealing with a special predicate. *)
        specialPredicate(term, unifier, succ, notSpecial, localFail, globalFail)
    end;
    
(* Takes a program, a query and two continuations. If the query is satisfiable 
   by the program, then the first continuation is called with the most general 
   unifier that satisfies it. If not, then the second continuation is called 
   with unit. *)
fun executeQuery( program, (Query(xs)), k1, k2, k3 ) = 
    let fun executeQueryTerms( [], unifier, k1, _ ) = k1( unifier )
          | executeQueryTerms( (x::xs), unifier, k1, k2 ) = 
            (* Executes the next term. *)
            let fun m1( newUnifier, k ) = 
                        executeQueryTerms( xs, newUnifier, k1, k )
            in
                findUnifier( program, x, unifier, m1, k2, k3 )
            end
    in
        executeQueryTerms( xs, (Unifier([])), k1, k2 )
    end;

(* Takes a program and a list of queries. Returns true if all of the queries 
   can be satisfied by the program. *)
fun executeQueries( _, [] ) = true
  | executeQueries( program, (x::xs) ) = 
    let val timer = Timer.startCPUTimer()
        fun m1( unifier ) = ( 
            printQuery( x ); 
            print "\n";
            (* Remove bindings which don't feature a variable. *)
            let val reducedUnifier = cleanUnifier( unifier ) in
            (* Perform variable substitutions. *)
            let val subUnifier = substituteUnifier( reducedUnifier ) in (
                (* Print the Unifier. *)
                printUnifier( subUnifier ) ;
                print "\n"
            );
            let val {usr, sys} = Timer.checkCPUTimer( timer )
            in
                print( Time.toString( Time.+( usr, sys ) ) );
                print "\n"
            end
            end end;
            executeQueries( program, xs) 
        )
        fun m2() = ( 
            printQuery( x ); 
            print "\nQuery not satisfiable.\n"; 
            false 
        ) 
    in
        executeQuery( program, x, m1, m2, m2 )
    end;
