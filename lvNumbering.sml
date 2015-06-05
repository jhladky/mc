signature LV_NUMBERING = sig
    val lvNumbering : IlocUtil.iloc_info list -> Iloc.program
end

structure LvNumbering :> LV_NUMBERING = struct
open Util
open Iloc
open IlocUtil

val insert = HashTable.insert

fun nextNum nextNum = (!nextNum) before nextNum := (!nextNum) + 1
fun exprToStr opc nums = opToStr opc ^ ", " ^ foldd ", " iToS nums
(* TODO: Double check these later. *)
fun replaceOK opc = not (has opc [OP_LOADRET, OP_NEW, OP_LOADGLOBAL])


val decompose =
 fn i as INS_RRR {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_RIR {opcode=opc, immed=n, ...} => (opc, [n], getST i)
  | i as INS_RRI {opcode=opc, immed=n, ...} => (opc, [n], getST i)
  | i as INS_RRC {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_RIC {opcode=opc, immed=n, ...} => (opc, [n], getST i)
  | i as INS_CLL {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_SIR {opcode=opc, immed=n, ...} => (opc, [n], getST i)
  | i as INS_NEW {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_RR  {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_IR  {opcode=opc, immed=n, ...} => (opc, [n], getST i)
  | i as INS_RI  {opcode=opc, immed=n, ...} => (opc, [n], getST i)
  | i as INS_SR  {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_RS  {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_L   {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_R   {opcode=opc, ...}          => (opc, [], getST i)
  | i as INS_X   {opcode=opc}               => (opc, [], getST i)


(* Get the value number for a register. If it doesn't exist, create one. *)
fun numberSource next vtn reg =
    case HashTable.find vtn (regToStr reg) of
        SOME (n, _) => n
      | NONE =>
        let val n = nextNum next in insert vtn (regToStr reg, (n, NONE)); n end


(* Get the value number for a constant. If it doesn't exist, create one. *)
fun numberConstant next vtn c =
    case HashTable.find vtn (iToS c) of
        SOME (n, _) => n
      | NONE =>
        let val n = nextNum next in insert vtn (iToS c, (n, NONE)); n end


(* Insert (possibly overwriting the old value) a new value number for the
 * target. *)
fun numberTarget next vtn tgt =
    let val n = nextNum next in insert vtn (regToStr tgt, (n, NONE)); n end


fun numberVals ii next vtn i =
    let
        val (opc, constants, (sources, tOpt)) = decompose i
        val nums = map (numberConstant next vtn) constants
                   @ map (numberSource next vtn) sources
    in
        case (replaceOK opc, tOpt, HashTable.find vtn (exprToStr opc nums)) of
            (* The instruction has a target, and a value already exists
             * for it, and it has already been given a register. Replace
             * the instruction with a move and update the value number
             * in the vtn. *)
            (true, SOME tgt, SOME (n, SOME dest)) =>
            let
                val newI = INS_RR {opcode=OP_MOV, r1=dest, dest=tgt}
            in
                insert vtn (regToStr tgt, (n, SOME dest));
                print ("Replacing " ^ insToStr i ^ " with " ^ insToStr newI ^ "\n");
                newI
            end
          (* The instruction has a target, and a value already exists for it,
           * but it has not been given a register. So update the vtn for the
           * new register and the target. *)
          | (true, SOME tgt, SOME (n, NONE)) =>
            let
                val dest = nextReg ii
                val newI = INS_RR {opcode=OP_MOV, r1=dest, dest=tgt}
            in
                insert vtn (exprToStr opc nums, (n, SOME dest));
                insert vtn (regToStr tgt, (n, SOME dest));
                print ("Replacing " ^ insToStr i ^ " with " ^ insToStr newI ^ "\n");
                newI
            end
          (* The instruction has a target, but a value goes not exist for it.
           * Insert one into the vtn. *)
          | (true, SOME tgt, NONE) =>
            let
                val n = numberTarget next vtn tgt
            in
                HashTable.insert vtn (exprToStr opc nums, (n, NONE));
                i
            end
          (* The instruction does not have a target. Do nothing. *)
          | (true, NONE, _) => i
          (* The instruction cannot be replaced. Do nothing. *)
          | (false, _, _) => i
    end


fun addValMoves ii vtn ((i, repl), ins) =
    ins @ [i]
    (* let *)
    (*     val (sources, tOpt) = getST i *)
    (*     val opc = getOpcode i *)
    (*     val skipRef = ref false *)
    (*     val sourceNums = List.map (#1 o HashTable.lookup vtn o iToS) sources *)
    (*                      handle Fail _ => [] before skipRef := true *)
    (* in *)
    (*     if repl andalso not (!skipRef) *)
    (*     then let *)
    (*         val (n, dOpt) = HashTable.lookup vtn (exprToStr opc sourceNums) *)
    (*     in *)
    (*         case dOpt of *)
    (*             NONE => ins @ [i] *)
    (*           | SOME dest => *)
    (*             (print ("Added move from " ^ iToS (valOf tOpt) ^ " to " ^ iToS dest ^ "\n"); *)
    (*              ins @ [i, INS_RR {opcode=OP_MOV, r1=valOf tOpt, dest=dest}]) *)
    (*     end *)
    (*     else ins @ [i] *)
    (* end *)


(* TODO: Debugging functions, remove later. *)
fun optToStr regOpt = case regOpt of NONE => "NONE" | SOME r => "SOME " ^ iToS r
fun printPair (expr, (number, regOpt)) =
    print (expr ^ " -> (" ^ iToS number ^ ", " ^ optToStr regOpt ^ ")\n")
fun printVtn id vtn = List.app printPair (HashTable.listItemsi vtn)


fun optBB ii node =
    let
        val (id, ins) = Cfg.getData node
        val _ = print (id ^ ":\n")
        val vtn = mkHt () (* Value-to-Number map *)
        val nextNum = ref 0
        val newIns = map (numberVals ii nextNum vtn) ins
        (* val newIns = foldl (addValMoves ii vtn) [] newIns *)
    in
        printVtn id vtn;
        (id, ins) before
        (* (id, newIns) before *)
        print "------------\n"
    end


fun optFunc (ii as II {id=id, cfg=cfg, ...}) =
    (Cfg.app (optBB ii) cfg; (id, cfg))


fun lvNumbering iis = map optFunc iis

end
