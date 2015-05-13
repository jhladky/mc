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


fun printAst fname ast =
    let
        val ots = openOut (fname ^ ".ast")
    in
        output (ots, Ast.programToStr ast);
        closeOut ots
    end


fun dumpIL fname st ast =
    let
        val ots = openOut (fname ^ ".il")
        val printDecl = fn (id, _) => output (ots, "@function " ^ id ^ "\n")
        val iloc = Ast2Iloc.ast2Iloc st ast
    in
        app printDecl iloc;
        output (ots, "\n");
        output (ots, Iloc.programToStr iloc);
        closeOut ots
    end


fun printAsm fname st ast =
    let
        val ots = openOut (fname ^ ".s")
    in
        output (ots, TargetAmd64.programToStr
                         (Iloc2Amd64.iloc2Amd64 st (Ast2Iloc.ast2Iloc st ast)));
        closeOut ots
    end


fun compile fname st ast =
    let
        val ots = openOut (fname ^ ".s")
    in
        output (ots, TargetAmd64.programToStr (
                    RegAlloc.regAlloc (Iloc2Amd64.iloc2Amd64 st (
                                            Ast2Iloc.ast2Iloc st ast))));
        closeOut ots
    end


fun getFname path =
    List.last (String.tokens (fn c => c = #"/")
                             (hd (String.tokens (fn c => c = #".") path)))


fun main () =
    let
        val opts as {file=filepath, ...} = parseArgs ()
        val fname = getFname filepath
        val ins = openIn filepath
        val ast = json2AST ins
        val st = SymbolTable.mkSymbolTable filepath ast
    in
        Static.staticCheck filepath st ast;
        if #printAst opts then printAst fname ast
        else if #dumpIL opts then dumpIL fname st ast
        else if #noRegAlloc opts then printAsm fname st ast
        else compile fname st ast;
        closeIn ins;
        OS.Process.exit OS.Process.success
    end

end

val _ = Main.main ()
