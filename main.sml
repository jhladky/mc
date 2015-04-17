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


fun dumpIL file ast =
    let
        val ots = TextIO.openOut ((hd (String.tokens (fn c => c = #".")
                                                     file)) ^ ".il");
    in
        printCfg ots (Ast2Cfg.ast2Cfg ast);
        TextIO.closeOut ots
    end


fun main () =
    let
        val {printAst=p, dumpIL=d, file=f} = parseArgs ();
        val ins = TextIO.openIn f;
        val ast = json2AST ins;
        val _ = TextIO.closeIn ins;
    in
        if p then PrintAst.printAst ast
        else (
            StaticCheck.staticCheck f ast;
            if d then dumpIL f ast else ()
        );
        OS.Process.exit OS.Process.success
    end

val _ = main ();
