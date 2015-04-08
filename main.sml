fun printUsage () =
    (TextIO.output (TextIO.stdErr, "Usage: mc [-printAST] <filename>\n");
     OS.Process.exit OS.Process.failure)
;

fun parseArgs () =
    let
        val args = CommandLine.arguments ();
        val _ = if length args = 0 then printUsage () else ();
    in
        if (hd args = "-printAST") then
            {printAST=true, file=List.nth (args, 1)}
        else {printAST=false, file=hd args}
    end
;
  
fun main () =
    let
        val {printAST=printAST, file=file} = parseArgs ();
        val ins = TextIO.openIn file;
    in
        (if printAST then (* printAST.printAST (json2AST ins *) ()
         else staticCheck.staticCheck file (json2AST ins);
         TextIO.closeIn ins)
    end
;

val _ = main ();
