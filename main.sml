fun printUsage () =
    (TextIO.output (TextIO.stdErr, "Usage: mc [-printAST] <filename>\n");
     OS.Process.exit OS.Process.failure)
;

(*This should use an array at some point*)
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
        val ast = json2AST ins;
        val _ = TextIO.closeIn ins;
    in
        if printAST then (
            PrintAST.printAST ast;
            OS.Process.exit OS.Process.success
        ) else (
            StaticCheck.staticCheck file ast;
            printCfg (Ast2Cfg.ast2Cfg ast);
            OS.Process.exit OS.Process.success
        )
    end
;

val _ = main ();
