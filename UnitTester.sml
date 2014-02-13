structure UnitTester =
  struct
    val testsRun = ref 0;
    val testsPassed = ref 0;
    fun printResultPoly testName actualResult expectedResult eqTest = (
      ( testsRun := (!testsRun) + 1 );
      if( eqTest( actualResult, expectedResult ) ) then ( 
        print "Test "; 
        print ( Int.toString (!testsRun) ); 
        print ": \""; 
        print testName; 
        print "\" passed.\n";
        ( testsPassed := (!testsPassed) + 1 )
      )
      else ( 
        print "Test ";
        print ( Int.toString (!testsRun) ); 
        print ": \"";
        print testName;
        print "\" FAILED!!!!!\n"
      )
    );
    fun printResult testName actualResult expectedResult = (
      printResultPoly testName actualResult expectedResult op=
    );
    fun conclude() = ( 
      print ( Int.toString (!testsPassed) ); 
      print " out of ";
      print ( Int.toString (!testsRun) );
      print " tests passed.\n";
      print ( Int.toString ( (!testsRun) - (!testsPassed) ) );
      print " tests failed.\n";
      testsRun := 0;
      testsPassed := 0
    );

  end;
    
val _ = (
    use "TopLevel.sml";
    use "Utility_UnitTests.sml";
    use "Interpreter_UnitTests.sml"
);

fun unitTest() = (
    unitTestUtility();
    unitTestInterpreter();
    UnitTester.conclude()
);