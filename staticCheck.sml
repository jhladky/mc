use "json2Ast.sml";

fun makeHt () = HashTable.mkTable (HashString.hashString, op =) (10, Fail "");


(*we're going to need to define a datatype representing the each type in the mini language*)

(*GLOBALS*)
val types = makeHt ();
val globalVars = makeHt ();

fun staticCheck file =
    let
        val PROGRAM {decls=decls, funcs=funcs, types=types} = json2Ast file;
    in
        ()
    end
;

val _ = staticCheck "tests/1.json";
