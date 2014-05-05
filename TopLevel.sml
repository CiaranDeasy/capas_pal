(********* Author: Ciaran Deasy    **********)
(********* cfd27@cam.ac.uk ******************)
(********* Part II Project ******************)
(********* University of Cambridge **********)

(*******************************************************************************
This source file acts as the top-level for the Prolog interpreter/compiler. It
imports all of the source files, and provides a single function that takes a 
filename and runs the file through the interpreter to output a result.
*******************************************************************************)

(* Import the source files. *)
val _ = ( 
        use "Utility.sml";
        use "Datatypes.sml";
        use "Lexer.sml";
        use "Parser_RecDes.sml";
        use "Unification.sml";
        use "SpecialPredicates.sml";
        use "Interpreter.sml";
        use "Compiler.sml"
);

(* Single top-level function to interpret a Prolog source file. *)
fun interpret( filename ) = 
    let val fileInStream = TextIO.openIn( filename )
        val tokenStream = lex( fileInStream )
        val x = TextIO.closeIn( fileInStream )
        val parsed = parseStart( tokenStream )
    in
        executeQueries( parsed )
    end;

(* Single top-level function to compile a Prolog source file. *)
fun compile( inFilename, outFilename ) = 
    let val fileInStream = TextIO.openIn( filename )
        val tokenStream = lex( fileInStream )
        val x = TextIO.closeIn( fileInStream )
        val parsed as ( program, queries ) = parseStart( tokenStream )
    in
        compileProgram( outFilename, program, queries )
    end end;

