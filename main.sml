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


(*This is the proper dumpIL function*)
fun dumpIL file st ast =
    let
        val ots = openOut (file ^ ".il")
        val printDecl = fn (id, _) => output (ots, "@function " ^ id ^ "\n")
        val funcs = map (fn (l, cfg) => (l, Cfg.toList cfg))
                        (Ast2Iloc.ast2Iloc st ast)
    in
        app printDecl funcs;
        output (ots, "\n");
        app (fn bb => output (ots, Iloc.bbToStr bb))
            (List.concat (map #2 funcs));
        closeOut ots
    end


(* fun dumpIL file st ast = *)
(*     let *)
(*         val ots = stdOut *)
(*         val mapBB = fn (label, ins) => (label, "TEST " ^ label ^ "\n") *)
(*         val mapFunc = fn (label, bb) => Cfg.map mapBB bb *)
(*         val funcs = map mapFunc (Ast2Iloc.ast2Iloc st ast) *)
(*         val funcs = map (fn cfg => Cfg.toList cfg) funcs *)
(*     in *)
(*         app (fn (l, str) => print (l ^ "\n" ^ str)) (List.concat funcs) *)
(*     end *)


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
