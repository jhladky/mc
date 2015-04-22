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


local
    fun printDecl ots (Cfg.FUNCTION {id=id, ...}) =
        output (ots, "@function " ^ id ^ "\n")

    fun printNode ots (l, L) =
        (output (ots, l ^ ":\n");
         app (fn ins => output (ots, "\t" ^ (Iloc.toString ins) ^ "\n")) L)
in
    fun dumpIL file ast =
        let
            val ots = TextIO.openOut (file ^ ".il");
            val Cfg.PROGRAM {funcs=funcs, ...} = Ast2Cfg.ast2Cfg ast;
        in
            app (printDecl ots) funcs;
            output (ots, "\n");
            app (printNode ots) (List.concat (map Cfg.toList funcs));
            TextIO.closeOut ots
        end
end


local
    fun printIns ots i = output (ots, "\t" ^ (TargetAmd64.insToStr i) ^ "\n")

    fun printNode ots (l, L) = (output (ots, l ^ ":\n"); app (printIns ots) L)

    fun printFunc ots (TargetAmd64.FUNC {id=id, preamble=pre,
                                         epilogue=epi, body=body}) =
        (output (ots, id ^ ":\n");
         app (fn dve => output (ots, (TargetAmd64.dveToStr dve) ^ "\n")) pre;
         app (printNode ots) body;
         app (fn dve => output (ots, (TargetAmd64.dveToStr dve) ^ "\n")) epi)
in
    fun printAsm file ast =
        let
            (* val ots = TextIO.openOut (file ^ ".s") *)
            val ots = TextIO.stdOut;
        in
            app (printFunc ots) (Cfg2Amd64.cfg2Amd64 (Ast2Cfg.ast2Cfg ast));
            TextIO.closeOut ots
        end
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
