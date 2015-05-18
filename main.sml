signature MAIN = sig
    val main : unit -> unit
end

structure Main :> MAIN = struct
open TextIO
open Util


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


fun printAsm fname platform st ast =
    let
        val ots = openOut (fname ^ ".s")
    in
        output (ots, TargetAmd64.programToStr platform
                         (Iloc2Amd64.iloc2Amd64 st (Ast2Iloc.ast2Iloc st ast)));
        closeOut ots
    end


fun compile fname platform st ast =
    let
        val ots = openOut (fname ^ ".s")
    in
        output (ots, (TargetAmd64.programToStr platform
                      o RegAlloc.regAlloc
                      o Iloc2Amd64.iloc2Amd64 st
                      o Ast2Iloc.ast2Iloc st) ast);
        closeOut ots
    end


fun main () =
    let
        val args = CommandLine.arguments ()
        val fname = hd args
        val mode = List.nth (args, 1)
        val platform = if List.nth (args, 2) = "Darwin" then OS_X else LINUX
        val ins = openIn (fname ^ ".json")
        val ast = json2AST ins
        val st = SymbolTable.mkSymbolTable (fname ^ ".mini") ast
    in
        Static.staticCheck (fname ^ ".mini") st ast;
        if mode = "-printAst" then printAst fname ast
        else if mode = "-dumpIL" then dumpIL fname st ast
        else if mode = "-noRegAlloc" then printAsm fname platform st ast
        else compile fname platform st ast;
        closeIn ins;
        OS.Process.exit OS.Process.success
    end

end

val _ = Main.main ()
