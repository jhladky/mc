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
        val ots = TextIO.openOut (file ^ ".il");
        val printDecl = fn Cfg.FUNCTION {id=id, ...} =>
                           output (ots, "@function " ^ id ^ "\n");
        val Cfg.PROGRAM {funcs=funcs, ...} = Ast2Cfg.ast2Cfg ast;
    in
        app printDecl funcs;
        output (ots, "\n");
        app (fn bb => output (ots, Iloc.bbToStr bb))
            (List.concat (map Cfg.toList funcs));
        TextIO.closeOut ots
    end


fun printAsm file ast =
    let
        (* val ots = TextIO.openOut (file ^ ".s") *) val ots = TextIO.stdOut;
    in
        output (ots, TargetAmd64.programToStr
                         (Cfg2Amd64.cfg2Amd64 (Ast2Cfg.ast2Cfg ast)));
        TextIO.closeOut ots
    end


fun main () =
    let
        val {printAst=p, dumpIL=d, file=f} = parseArgs ();
        val fname = (hd (String.tokens (fn c => c = #".") f));
        val ins = TextIO.openIn f;
        val ast = json2AST ins;
    in
        TextIO.closeIn ins;
        if p
        then PrintAst.printAst ast
        else (
            StaticCheck.staticCheck f ast;
            if d
            then dumpIL fname ast
            else printAsm fname ast
        );
        OS.Process.exit OS.Process.success
    end

val _ = main ();
