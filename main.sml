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


fun stage2 st ast fname dumpIl mochiCompat noOpt
           noCopyProp noRegAlloc noStrip noLvn platform =
    let
        val iloc = Ast2Iloc.ast2Iloc st ast mochiCompat
        val iloc = if noOpt orelse noLvn then iloc
                   else LvNumbering.lvNumbering iloc
        val iloc = if noOpt orelse noCopyProp then iloc
                   else CopyProp.copyProp iloc
        val iloc = if noOpt orelse noStrip then iloc
                   else StripDeadCode.stripDeadCode iloc
    in
        if dumpIl then doDumpIl fname iloc
        else stage3 st iloc fname noRegAlloc platform
    end


fun main () =
    let
        val args = Array.fromList (CommandLine.arguments ())
        val fname = Array.sub (args, 0)
        val dumpIl = getBool (args, 1)
        val mochiCompat = getBool (args, 2)
        val noOpt = getBool (args, 3)
        val noCopyProp = getBool (args, 4)
        val noLvn = getBool (args, 5)
        val noStrip = getBool (args, 6)
        val noRegAlloc = getBool (args, 7)
        val staticCheck = getBool (args, 8)
        val platform = if Array.sub (args, 9) = "Darwin" then OS_X else LINUX
        val ins = openIn (fname ^ ".json")
        val ast = json2AST ins
        val st = SymbolTable.mkSymbolTable (fname ^ ".mini") ast
    in
        Static.staticCheck (fname ^ ".mini") st ast;
        if staticCheck then exit () else ();
        stage2 st ast fname dumpIl mochiCompat noOpt
               noCopyProp noRegAlloc noStrip noLvn platform;
        closeIn ins;
        exit ()
    end

end

val _ = Main.main ()
