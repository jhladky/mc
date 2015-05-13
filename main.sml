signature MAIN = sig
    val main : unit -> unit
end

structure Main :> MAIN = struct
open TextIO


(*This should use an array at some point*)
fun parseArgs () =
    let
        val args = CommandLine.arguments ()
    in
        (hd args,
         (case List.nth (args, 1) of
              "-printAst" =>
              {printAst=true, dumpIL=false, noRegAlloc=false}
            | "-dumpIL" =>
              {printAst=false, dumpIL=true, noRegAlloc=false}
            | "-noRegAlloc" =>
              {printAst=false, dumpIL=false, noRegAlloc=true}
            | _ => raise Fail "Bad Arguments")
         handle Subscript =>
                {printAst=false, dumpIL=false, noRegAlloc=false})
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


fun main () =
    let
        val (fname, opts) = parseArgs ()
        val ins = openIn (fname ^ ".json")
        val ast = json2AST ins
        val st = SymbolTable.mkSymbolTable (fname ^ ".mini") ast
    in
        Static.staticCheck (fname ^ ".mini") st ast;
        if #printAst opts then printAst fname ast
        else if #dumpIL opts then dumpIL fname st ast
        else if #noRegAlloc opts then printAsm fname st ast
        else compile fname st ast;
        closeIn ins;
        OS.Process.exit OS.Process.success
    end

end

val _ = Main.main ()
