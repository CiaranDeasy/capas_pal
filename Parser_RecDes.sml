(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains the functions that implement a recursive-descent 
parser, taking as input a list of tokens and returning an ML datastructure 
representing the Prolog program.
*******************************************************************************)

(* Takes a list of tokens produced by the lexer and returns a 
   ( Program, queries ) tuple. *)
fun parseStart( tokens ) = 
    let val x as (clauses, queries, tokens2) = parseLines( tokens )
    in
        if( eqToken( hd tokens2, EOF ) )
            then (Program(clauses), queries)
        else
            (Program([]), []) (*ERROR*)
    end
    
(* Returns a list of clauses, a list of queries, and a list of the remaining 
   tokens. *)
and parseLines( tokens ) =
    let val x as (clause, query, tokens2) = parseLine( tokens )
    in
        if( eqToken( hd tokens2, DOT ) ) 
            then 
                let val y as (clauses, queries, tokens3) = 
                        parseMoreLines( tl tokens2 )
                in
                    if( List.null( clause ) )
                        then ( clauses, ( ( hd query ) :: queries ), tokens3 )
                    else
                        ( ( ( hd clause ) :: clauses ), queries, tokens3 )
                end
        else 
            ([], [], tokens) (*ERROR*)
    end
    
(* Returns a list of clauses, a list of queries, and a list of the remaining 
   tokens. *)  
and parseMoreLines( ATOM(a)::tokens ) = parseLines( ATOM(a)::tokens )
  | parseMoreLines( VARIABLE(v)::tokens ) = parseLines( VARIABLE(v)::tokens )
  | parseMoreLines( LEFTSQ::tokens ) = parseLines( LEFTSQ::tokens )
  | parseMoreLines( INT(n)::tokens ) = parseLines( INT(n)::tokens )
  | parseMoreLines( COLONMINUS::tokens ) = parseLines( COLONMINUS::tokens )
  | parseMoreLines( EOF::tokens ) = ( [], [], EOF::tokens )

(* If the next line is a clause, returns a single-element clause list, an empty
   query list, and a list of the remaining tokens. 
   If the next line is a query, returns an empty clause list, a single-element 
   query list, and a list of the remaining tokens. *)  
and parseLine( COLONMINUS::tokens ) = 
    let val x as ( query, tokens2 ) = parseQuery( COLONMINUS::tokens )
    in
        ( [], [query], tokens2 )
    end
  | parseLine( ATOM(a)::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( ATOM(a)::tokens )
    in
        ( [clause], [], tokens2 )
    end
  | parseLine( VARIABLE(v)::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( VARIABLE(v)::tokens )
    in
        ( [clause], [], tokens2 )
    end
  | parseLine( LEFTSQ::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( LEFTSQ::tokens )
    in
        ( [clause], [], tokens2 )
    end
  | parseLine( INT(n)::tokens ) = 
    let val x as ( clause, tokens2 ) = parseClause( INT(n)::tokens )
    in
        ( [clause], [], tokens2 )
    end

(* Returns a clause and a list of the remaining tokens. *)
and parseClause( tokens ) =
    let val x as (head, tokens2) = parseTerm( tokens ) in
    let val y as (body, tokens3) = parseBody( tokens2 )
    in
        ( Clause( head, body ), tokens3 )
    end end

(* Returns a list of terms and a list of the remaining tokens. *)
and parseBody( COLONMINUS::tokens ) = parseTermList( tokens )
  | parseBody( DOT::tokens ) = ( [], (DOT::tokens) )

(* Returns a query and a list of the remaining tokens. *)
and parseQuery( COLONMINUS::tokens ) = 
    let val x as (terms, tokens2) = parseTermList( tokens )
    in
        ( scopeQuery( Query( terms ) ), tokens2 )
    end

(* Returns a list of terms and a list of the remaining tokens *)
and parseTermList( ATOM(a)::tokens ) = 
    let val x as (term, tokens2) = parseTerm( ATOM(a)::tokens ) in
    let val y as (isTerm, tokens3) = parseIsTerm( tokens2, term ) in
    let val z as (terms, tokens4) = parseMoreTerms( tokens3 )
    in
        ( (isTerm::terms), tokens4 )
    end end end
  | parseTermList( VARIABLE(v)::tokens ) = 
    let val x as (term, tokens2) = parseTerm( VARIABLE(v)::tokens ) in
    let val y as (isTerm, tokens3) = parseIsTerm( tokens2, term ) in
    let val z as (terms, tokens4) = parseMoreTerms( tokens3 )
    in
        ( (isTerm::terms), tokens4 )
    end end end
  | parseTermList( LEFTSQ::tokens ) = 
    let val x as (term, tokens2) = parseTerm( LEFTSQ::tokens ) in
    let val y as (isTerm, tokens3) = parseIsTerm( tokens2, term ) in
    let val z as (terms, tokens4) = parseMoreTerms( tokens3 )
    in
        ( (isTerm::terms), tokens4 )
    end end end
  | parseTermList( INT(i)::tokens ) = 
    let val x as (term, tokens2) = parseTerm( INT(i)::tokens ) in
    let val y as (isTerm, tokens3) = parseIsTerm( tokens2, term ) in
    let val z as (terms, tokens4) = parseMoreTerms( tokens3 )
    in
        ( (isTerm::terms), tokens4 )
    end end end
  | parseTermList( FLOAT(f)::tokens ) = 
    let val x as (term, tokens2) = parseTerm( FLOAT(f)::tokens ) in
    let val y as (isTerm, tokens3) = parseIsTerm( tokens2, term ) in
    let val z as (terms, tokens4) = parseMoreTerms( tokens3 )
    in
        ( (isTerm::terms), tokens4 )
    end end end
  | parseTermList( LEFTPAREN::tokens ) = 
    let val x as (term, tokens2) = parseTerm( LEFTPAREN::tokens ) in
    let val y as (isTerm, tokens3) = parseIsTerm( tokens2, term ) in
    let val z as (terms, tokens4) = parseMoreTerms( tokens3 )
    in
        ( (isTerm::terms), tokens4 )
    end end end
    (* TermList -> empty-string *)
  | parseTermList( tokens ) = ( [], tokens )

(* Returns a list of terms and a list of the remaining tokens *)
and parseMoreTerms( COMMA::tokens ) = parseTermList( tokens )
  | parseMoreTerms( DOT::tokens ) = ( [], DOT::tokens )
  | parseMoreTerms( RIGHTPAREN::tokens ) = ( [], RIGHTPAREN::tokens )
  | parseMoreTerms( RIGHTSQ::tokens ) = ( [], RIGHTSQ::tokens )
  | parseMoreTerms( PIPE::tokens ) = ( [], PIPE::tokens )
  | parseMoreTerms( tokens ) = ( printTokenStream( tokens ); ( [], [] ) )

(* Returns a term and a list of the remaining tokens. *)
and parseTerm( ATOM(a)::tokens ) = 
    let val x as ( args, tokens2 ) = parseArgs( tokens )
    in 
        ( Term( Functor( a ), args ), tokens2 )
    end
  | parseTerm( LEFTSQ::tokens ) = 
    let val x as (terms, tokens2) = parseTermList( tokens ) in
    let val y as (tail, tokens3) = parseMoreList( tokens2 ) in
    let fun buildList [] = tail
          | buildList (element::elements) = 
                Term( Functor("."), [ element, buildList( elements ) ] )
    in
        ( ( buildList terms ), tokens3 )
    end end end
  | parseTerm( VARIABLE(v)::tokens ) = 
    let val x as (arith, tokens2) = parseArith( VARIABLE(v)::tokens )
    in
        parseMoreArith( tokens2, arith )
    end
  | parseTerm( INT(n)::tokens ) = 
    let val x as (arith, tokens2) = parseArith( INT(n)::tokens )
    in
        parseMoreArith( tokens2, arith )
    end
  | parseTerm( FLOAT(f)::tokens ) =
    let val x as (arith, tokens2) = parseArith( FLOAT(f)::tokens )
    in
        parseMoreArith( tokens2, arith )
    end
  | parseTerm( LEFTPAREN::tokens ) = 
    let val x as (arith, tokens2) = parseArith( LEFTPAREN::tokens )
    in
        parseMoreArith( tokens2, arith )
    end

(* Returns a list of terms and a list of the remaining tokens. *)
and parseArgs( LEFTPAREN::tokens ) =
    let val x as (terms, tokens2) = parseTermList( tokens )
    in
        if( eqToken( hd tokens2, RIGHTPAREN ) )
            then ( terms, ( tl tokens2 ) )
        else
            ( terms, ( tl tokens2 ) ) (*ERROR*)
    end
  | parseArgs( tokens ) = ( [], tokens ) 

(* Returns a term and a list of the remaining tokens *)
and parseMoreList( RIGHTSQ::tokens ) = ( Term( Functor("[]"), [] ), tokens )
  | parseMoreList( PIPE::tokens ) = 
    let val x as ( term, tokens2 ) = parseTail( tokens )
    in
        if( eqToken( hd tokens2, RIGHTSQ ) )
            then ( term, ( tl tokens2 ) )
        else
            ( term, ( tl tokens2 ) ) (* ERROR *)
    end

(* Returns a term and a list of the remaining tokens. *)
and parseTail( VARIABLE(v)::tokens ) = ( Variable( v, 0 ), tokens )
  | parseTail( LEFTSQ::tokens ) =
    let val x as (terms, tokens2) = parseTermList( tokens ) in
    let val y as (tail, tokens3) = parseMoreList( tokens2 ) in
    let fun buildList [] = tail
          | buildList (element::elements) = 
                Term( Functor("."), [ element, buildList( elements ) ] )
    in
        ( ( buildList terms ), tokens3 )
    end end end

(* Returns a term and the remaining tokens. *)
and parseIsTerm( IS::tokens, prevTerm ) = 
    let val x as (nextTerm, tokens2) = parseTerm( tokens ) in
    let val y as (nextIsTerm, tokens3) = parseIsTerm( tokens2, nextTerm )
    in
        ( Term( Functor("is"), [ prevTerm, nextIsTerm ] ), tokens3 )
    end end
  | parseIsTerm( tokens, prevTerm ) = ( prevTerm, tokens )

(* Returns a Term and the remaining tokens. *)
and parseArith( tokens ) = 
    let val x as (arithTerm, tokens2) = parseArithTerm( tokens )
    in
        parseMoreArithTerms( tokens2, arithTerm )
    end
    
(* Takes the current tokens and an already-parsed arithmetic expression.
   Returns a Term and a list of the remaining tokens. *)
and parseMoreArith( LESS::tokens, prevArith ) = 
    let val x as (nextArith, tokens2) = parseArith( tokens ) in
        ( Term( Functor("<"), [ prevArith, nextArith ] ), tokens2 )
    end
  | parseMoreArith( GREATER::tokens, prevArith ) = 
    let val x as (nextArith, tokens2) = parseArith( tokens ) in
        ( Term( Functor(">"), [ prevArith, nextArith ] ), tokens2 )
    end
  | parseMoreArith( EQUALS::tokens, prevArith ) = 
    let val x as (nextArith, tokens2) = parseArith( tokens ) in
        ( Term( Functor("="), [ prevArith, nextArith ] ), tokens2 )
    end
    (* MoreArith -> empty-string *)
  | parseMoreArith( tokens, prevArith ) = ( prevArith, tokens )
    
(* Returns a Term and the remaining tokens. *)
and parseArithTerm( tokens ) = 
    let val x as (factor, tokens2) = parseFactor( tokens )
    in
        parseMoreFactors( tokens2, factor )
    end
    
(* Takes the current tokens and an already-parsed arithmetic term.
   Returns a Term and the remaining tokens. *)
and parseMoreArithTerms( PLUS::tokens, arithTerm ) =
    let val x as (arithTerms, tokens2) = parseArith( tokens )
    in
        ( Term( Functor("+"), [ arithTerm, arithTerms ] ), tokens2 )
    end
  | parseMoreArithTerms( MINUS::tokens, arithTerm ) =
    let val x as (arithTerms, tokens2) = parseArith( tokens )
    in
        ( Term( Functor("-"), [ arithTerm, arithTerms ] ), tokens2 )
    end
  | parseMoreArithTerms( tokens, arithTerm ) = ( arithTerm, tokens )
    
(* Returns a term and the remaining tokens. *)
and parseFactor( LEFTPAREN::tokens ) = 
    let val x as (arith, tokens2) = parseArith( tokens )
    in
        if( eqToken( hd tokens2, RIGHTPAREN ) )
            then ( arith, tl tokens2 )
        else
            ( arith, tl tokens2 ) (*ERROR*)
    end
  | parseFactor( INT(i)::tokens ) = ( IntTerm( i ), tokens )
  | parseFactor( FLOAT(f)::tokens ) = ( FloatTerm( f ), tokens )
  | parseFactor( VARIABLE(v)::tokens ) = ( Variable(v, 0), tokens )
  

and parseMoreFactors( MULT::tokens, prevFactor ) = 
    let val x as (nextFactor, tokens2) = parseFactor( tokens )
    in
        parseMoreFactors( tokens2, 
                Term( Functor("*"), [ prevFactor, nextFactor ] ) )
    end
  | parseMoreFactors( DIV::tokens, prevFactor ) = 
    let val x as (nextFactor, tokens2) = parseFactor( tokens )
    in
        parseMoreFactors( tokens2, 
                Term( Functor("/"), [ prevFactor, nextFactor ] ) )
    end
  | parseMoreFactors( MOD::tokens, prevFactor ) = 
    let val x as (nextFactor, tokens2) = parseFactor( tokens )
    in
        parseMoreFactors( tokens2, 
                Term( Functor("%"), [ prevFactor, nextFactor ] ) )
    end
  | parseMoreFactors( tokens, factor ) = ( factor, tokens );
  