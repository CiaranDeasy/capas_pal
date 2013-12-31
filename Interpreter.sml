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
  
(* Takes two lists, and removes from the first any elements that occur in the second. *)
fun removeDuplicates [] ys = []
  | removeDuplicates (x::xs) ys = if ( member x ys ) then removeDuplicates xs ys else (x::( removeDuplicates xs ys ));



fun unitTest() = 
        let val testNo = ref 1 in 
		let fun printTestResult actualResult expectedResult = (
		    if( actualResult = expectedResult )
		        then ( print "Test "; print ( Int.toString (!testNo) ); 
			    print " passed.\n" )
			else ( print "Test "; print ( Int.toString (!testNo) ); 
			    print " FAILED!!!!!\n" ); ( testNo := (!testNo) + 1 ) 
		) in
			
		( printTestResult ( member 1 [1,2,3] ) true;
          printTestResult ( member 3 [1,2,3] ) true;
          printTestResult ( member 4 [1,2,3] ) false;
          printTestResult ( member 4 [] ) false;
          printTestResult ( removeDuplicates [2,3,4] [1,2,3] ) [4];
          printTestResult ( removeDuplicates [] [1,2,3] ) [];
          printTestResult ( removeDuplicates [2,3,4] [] ) [2,3,4]
          )
		end end;