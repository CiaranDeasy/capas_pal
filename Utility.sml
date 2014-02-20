(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file contains some general-purpose functions used by the Prolog
interpreter/compiler.
*******************************************************************************)

(* Takes a list, a value and an equality test. Returns true if the value is in 
   the list, according to the equality test. *)
fun memberPoly x [] _ = false
  | memberPoly x (y::ys) eqTest = eqTest(x, y) orelse memberPoly x ys eqTest;

(* Takes a list and a value. Returns true if the value is in the list. *)
fun member x ys = memberPoly x ys op=;
  
(* Takes two lists, and removes from the first any elements that occur in the 
   second. *)
fun removeWithBlacklist [] ys = []
  | removeWithBlacklist (x::xs) ys = 
        if ( member x ys ) 
		    then removeWithBlacklist xs ys 
	    else (x::( removeWithBlacklist xs ys ));

(* Takes two lists of equal length, and returns a list of 2-tuples, where the 
   i'th tuple contains the i'th element of each input list. *)
fun zip [] [] = []
  | zip (x::xs) (y::ys) = (x,y) :: (zip xs ys);

(* Return the first and second elements of an input 2-tuple, respectively. *)
fun first (a, b) = a;
fun second (a, b) = b;

(* Takes a list of bools and returns the result of ANDing them all together. *)
fun andList [] = true
  | andList (x::xs) = x andalso andList xs;

(* Takes a 2-tuple and returns the a 2-tuple with the same elements in reverse 
   order. *)
fun flip( x, y ) = ( y, x );

(* Polymorphic equality test for ordered lists *)
fun eqOrderedList _ ( [], [] ) = true
  | eqOrderedList _ ( xs, [] ) = false
  | eqOrderedList _ ( [], ys ) = false
  | eqOrderedList eqTest ( (x::xs), (y::ys) ) = 
        eqTest( x, y ) andalso eqOrderedList eqTest ( xs, ys );

(*fun op=( Term( ( f1 : functor_t ), args1 ), Term( ( f2 : functor_t ), args2 ) 
) = ( op=( f1, f2 ) ) andalso ( args1 = args2 );*)
		  
(* Polymorphic equality test for 2-tuples *)
fun eqTwoTuple eqTestA eqTestB ( ( x1, y1 ), (x2, y2 ) ) = 
        eqTestA( x1, x2 ) andalso eqTestB( y1, y2 );
		  
(* Polymorphic equality test for unordered lists. *)
fun eqUnorderedList _ ( [], [] ) = true
  | eqUnorderedList _ ( [], ys ) = false
  | eqUnorderedList _ ( xs, [] ) = false
  | eqUnorderedList eqTest ( (x::xs), ys ) = 
    (* For each element of the first list, step through the second list and 
	   match it to an equal element. Remove both elements and repeat. *)
    let fun remove( x, [] ) = ( false, [] )
	      | remove( x, (y::ys) ) = 
		        if( eqTest(x, y) )
			        then ( true, ys )
				else
				    let val result = remove( x, ys ) in
					    if( first result )
						    then ( true, (y::( second result )) )
					    else result 
					end in
	let val result = remove( x, ys ) in
        (first result) andalso eqUnorderedList eqTest ( xs, ( second result ) )
	end end;
		  
(* Polymorphic equality test for unordered lists, ignoring duplicates. *)
fun eqUnorderedListIgnoreDups _ ( [], [] ) = true
  | eqUnorderedListIgnoreDups _ ( [], ys ) = false
  | eqUnorderedListIgnoreDups _ ( xs, [] ) = false
  | eqUnorderedListIgnoreDups eqTest ( (x::xs), ys ) = 
    (* For each element of the first list, step through the second list and 
	   match it to an equal element. Remove both elements and repeat. *)
    let fun removeAll( x, [] ) = []
	      | removeAll( x, (y::ys) ) = 
		        if( eqTest(x, y) ) then 
                    removeAll( x, ys )
				else
                    ( y::( removeAll( x, ys ) ) )
    in
        if( memberPoly x ys eqTest ) then
            eqUnorderedListIgnoreDups eqTest 
              ( ( removeAll( x, xs ) ), ( removeAll( x, ys ) ) )
        else
            false
	end;
