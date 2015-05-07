signature MAIN = sig
    val main : unit -> unit
end

structure Main :> MAIN = struct
open TextIO

fun printUsage () =
    (output (stdErr, "Usage: mc -printAst <filename>\n" ^
                     "       mc -noRegAlloc <filename>\n" ^
                     "       mc -dumpIL <filename>\n");
     OS.Process.exit OS.Process.failure)


(*This should use an array at some point*)
fun parseArgs () =
    let
        val args = CommandLine.arguments ()
        val opt = hd args
        val _ = if length args = 0 then printUsage () else ()
    in
        if opt = "-printAst"
        then {printAst=true, dumpIL=false,
              noRegAlloc=false, file=List.nth (args, 1)}
        else if opt = "-dumpIL"
        then {printAst=false, dumpIL=true,
              noRegAlloc=false, file=List.nth (args, 1)}
        else if opt = "-noRegAlloc"
        then {printAst=false, dumpIL=false,
              noRegAlloc=true, file=List.nth (args, 1)}
        else {printAst=false, dumpIL=false,
              noRegAlloc=false, file=hd args}
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
        val printDecl = fn (id, _) => output (ots, "@function " ^ id ^ "\n")
        val iloc = Ast2Iloc.ast2Iloc st ast
    in
        app printDecl iloc;
        output (ots, "\n");
        output (ots, Iloc.programToStr iloc);
        closeOut ots
    end


fun printAsm file st ast =
    let
        (* val ots = openOut (file ^ ".s") *) val ots = stdOut
    in
        output (ots, TargetAmd64.programToStr
                         (Cfg2Amd64.cfg2Amd64 st (Ast2Iloc.ast2Iloc st ast)));
        closeOut ots
    end


fun compile file st ast =
    let
        (* val ots = openOut (file ^ ".s") *) val ots = stdOut
        val asm = Cfg2Amd64.cfg2Amd64 st (Ast2Iloc.ast2Iloc st ast)
    in
        RegAlloc.regAlloc asm;
        closeOut ots
    end


fun main () =
    let
        val opts as {file=f, ...} = parseArgs ()
        val fname = (hd (String.tokens (fn c => c = #".") f))
        val ins = openIn f
        val ast = json2AST ins
        val st = SymbolTable.mkSymbolTable f ast
    in
        Static.staticCheck f st ast;
        if #printAst opts then printAst fname ast
        else if #dumpIL opts then dumpIL fname st ast
        else if #noRegAlloc opts then printAsm fname st ast
        else compile fname st ast;
        closeIn ins;
        OS.Process.exit OS.Process.success
    end

end

val _ = Main.main ()
