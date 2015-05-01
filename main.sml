signature MAIN = sig
    val main : unit -> unit
end

structure Main :> MAIN = struct
open TextIO

fun printUsage () =
    (output (stdErr, "Usage: mc [-printAST|-dumpIL] <filename>\n");
     OS.Process.exit OS.Process.failure)


(*This should use an array at some point*)
fun parseArgs () =
    let
        val args = CommandLine.arguments ()
        val _ = if length args = 0 then printUsage () else ()
    in
        if (hd args = "-printAST")
        then {printAst=true, dumpIL=false, file=List.nth (args, 1)}
        else if (hd args = "-dumpIL")
        then {printAst=false, dumpIL=true, file=List.nth (args, 1)}
        else {printAst=false, dumpIL=false, file=hd args}
    end


fun printAst file ast =
    let
        val ots = stdOut
    in
        output (ots, Ast.programToStr ast);
        closeOut ots
    end


fun dumpIL file st ast =
    let
        val ots = openOut (file ^ ".il")
        val printDecl = fn Cfg.FUNCTION {id=id, ...} =>
                           output (ots, "@function " ^ id ^ "\n")
        val funcs = Ast2Cfg.ast2Cfg st ast
    in
        app printDecl funcs;
        output (ots, "\n");
        app (fn bb => output (ots, Iloc.bbToStr bb))
            (List.concat (map Cfg.toList funcs));
        closeOut ots
    end


fun printAsm file st ast =
    let
        (* val ots = openOut (file ^ ".s") *) val ots = stdOut
    in
        output (ots, TargetAmd64.programToStr
                         (Cfg2Amd64.cfg2Amd64 st (Ast2Cfg.ast2Cfg st ast)));
        closeOut ots
    end


fun main () =
    let
        val {printAst=p, dumpIL=d, file=f} = parseArgs ()
        val fname = (hd (String.tokens (fn c => c = #".") f))
        val ins = openIn f
        val ast = json2AST ins
        val st = SymbolTable.mkSymbolTable f ast
    in
        if p then printAst fname ast
        else (Static.staticCheck f st ast;
              if d then dumpIL fname st ast else printAsm fname st ast);
        closeIn ins;
        OS.Process.exit OS.Process.success
    end

end

val _ = Main.main ();
