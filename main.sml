signature MAIN = sig
    val main : unit -> unit
end

structure Main :> MAIN = struct
open TextIO
open Util


fun exit () = OS.Process.exit OS.Process.success
fun getBool pair = valOf (Bool.fromString (Array.sub pair))


fun stage3 st iloc fname noRegAlloc platform =
    let
        val asm = Iloc2Amd64.iloc2Amd64 st iloc
        val asm = if noRegAlloc then asm else RegAlloc.regAlloc asm
        val ots = openOut (fname ^ ".s")
    in
        output (ots, TargetAmd64.programToStr platform asm);
        closeOut ots
    end


fun doDumpIl fname iloc =
    let
        val ots = openOut (fname ^ ".il")
        val outDecl = fn (id, _) => output (ots, "@function " ^ id ^ "\n")
    in
        app outDecl iloc;
        output (ots, "\n");
        output (ots, Iloc.programToStr iloc);
        closeOut ots;
        exit ()
    end


fun stage2 st ast fname dumpIl noOpt noCopyProp noRegAlloc platform =
    let
        val iloc = Ast2Iloc.ast2Iloc st ast
        val iloc = if not noOpt andalso not noCopyProp
                   then CopyProp.copyProp iloc
                   else iloc
    in
        if dumpIl then doDumpIl fname iloc
        else stage3 st iloc fname noRegAlloc platform
    end


fun main () =
    let
        val args = Array.fromList (CommandLine.arguments ())
        val fname = Array.sub (args, 0)
        val dumpIl = getBool (args, 1)
        val noOpt = getBool (args, 2)
        val noCopyProp = getBool (args, 3)
        val noRegAlloc = getBool (args, 4)
        val staticCheck = getBool (args, 5)
        val platform = if Array.sub (args, 6) = "Darwin" then OS_X else LINUX
        val ins = openIn (fname ^ ".json")
        val ast = json2AST ins
        val st = SymbolTable.mkSymbolTable (fname ^ ".mini") ast
    in
        Static.staticCheck (fname ^ ".mini") st ast;
        if staticCheck then exit () else ();
        stage2 st ast fname dumpIl noOpt noCopyProp noRegAlloc platform;
        closeIn ins;
        exit ()
    end

end

val _ = Main.main ()
