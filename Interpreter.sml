datatype functor_t = Functor of string;
datatype term_t = Term of functor_t * term_t list
                | Variable of string;
datatype clause_t = Clause of term_t * term_t list;
datatype program_t = Program of clause_t list;
datatype query_t = Query of term_t list;
datatype binding_t = Binding of term_t * term_t;
datatype unifier_t = Unifier of binding_t list;

val program = Program( [
Clause( Term( Functor( "bear" ), [Term( Functor( "pooh" ), [] )] ), [] ), 
Clause( Term( Functor( "likes" ), [Term( Functor( "pooh" ), [] ), Term( Functor( "honey" ), [] )] ), [] ), 
Clause( Term( Functor( "likes" ), [Term( Functor( "pooh" ), [] ), Term( Functor( "honey" ), [] )] ), [Term( Functor( "bear" ), [Term( Functor( "pooh" ), [] )] ), Term( Functor( "real" ), [Term( Functor( "bear" ), [] )] ), Term( Functor( "my" ), [Term( Functor( "term" ), [] )] )] )
] );
val queries = [Query ( [Term( Functor( "bear" ), [Variable( "_G1675" )] ), Term( Functor( "likes" ), [Variable( "_G1675" ), Term( Functor( "honey" ), [] )] )] )];


(* Takes a list and a value. Returns true if the value is in the list. *)
fun member x [] = false
  | member x (y::ys) = (x = y) orelse member x ys
  
(* Takes two lists, and removes from the first any elements that occur in the 
   second. *)
fun removeDuplicates [] ys = []
  | removeDuplicates (x::xs) ys = 
        if ( member x ys ) 
		    then removeDuplicates xs ys 
	    else (x::( removeDuplicates xs ys ));

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



fun unitTest() = 
        let val testNo = ref 1 in 
		let fun printResult actualResult expectedResult = (
		    if( actualResult = expectedResult )
		        then ( print "Test "; print ( Int.toString (!testNo) ); 
			    print " passed.\n" )
			else ( print "Test "; print ( Int.toString (!testNo) ); 
			    print " FAILED!!!!!\n" ); ( testNo := (!testNo) + 1 ) 
		) in
			
		( printResult ( member 1 [1,2,3] ) true;
          printResult ( member 3 [1,2,3] ) true;
          printResult ( member 4 [1,2,3] ) false;
          printResult ( member 4 [] ) false;
          printResult ( removeDuplicates [2,3,4] [1,2,3] ) [4];
          printResult ( removeDuplicates [] [1,2,3] ) [];
          printResult ( removeDuplicates [2,3,4] [] ) [2,3,4];
          printResult ( zip [1,2] [3,4] ) [(1,3),(2,4)];
          printResult ( zip [1] [2] ) [(1,2)];
          printResult ( zip [] [] ) [];
          printResult ( first (1,2) ) 1;
          printResult ( second (1,2) ) 2;
		  printResult ( andList [true, false, true] ) false;
		  printResult ( andList [true, true, true] ) true;
		  printResult ( andList [] ) true;
		  printResult ( flip (1,2) ) (2,1)
          )
		end end;