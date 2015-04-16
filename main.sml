fun printUsage () =
    (TextIO.output (TextIO.stdErr,
                    "Usage: mc [-printAST|-dumpIL] <filename>\n");
     OS.Process.exit OS.Process.failure)


(*This should use an array at some point*)
fun parseArgs () =
    let
        val args = CommandLine.arguments ();
        val _ = if length args = 0 then printUsage () else ();
    in
        if (hd args = "-printAST")
        then {printAst=true, dumpIL=false, file=List.nth (args, 1)}
        else if (hd args = "-dumpIL")
        then {printAst=false, dumpIL=true, file=List.nth (args, 1)}
        else {printAst=false, dumpIL=false, file=hd args}
    end


fun main () =
    let
        val {printAst=printAst, dumpIL=dumpIL, file=file} = parseArgs ();
        val ins = TextIO.openIn file;
        val ots = TextIO.openOut ((hd (String.tokens (fn c => c = #".")
                                                     file)) ^ ".il");
        val ast = json2AST ins;
        val _ = TextIO.closeIn ins;
        val exit = fn () => OS.Process.exit OS.Process.success;
    in
        if printAst
        then (PrintAst.printAst ast; exit ())
        else if dumpIL
        then (StaticCheck.staticCheck file ast;
              printCfg ots (Ast2Cfg.ast2Cfg ast);
              exit ())
        else (StaticCheck.staticCheck file ast; exit ())
    end

val _ = main ();
