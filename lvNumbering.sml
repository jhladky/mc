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
fun replaceOK opc = not (has opc [OP_LOADRET, OP_NEW,
                                  OP_LOADGLOBAL, OP_LOADAI])


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


(* The string option we pass back with the instruction indicates whether we
 * will have to add mov instruction later. *)
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
                print ("Replaced " ^ insToStr i ^ " with " ^ insToStr newI ^ "\n");
                (newI, NONE)
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
                print ("Replaced " ^ insToStr i ^ " with " ^ insToStr newI ^ "\n");
                (newI, NONE)
            end
          (* The instruction has a target, but a value goes not exist for it.
           * Insert one into the vtn. *)
          | (true, SOME tgt, NONE) =>
            let
                val n = numberTarget next vtn tgt
                val key = exprToStr opc nums
            in
                HashTable.insert vtn (key, (n, NONE)); (i, SOME key)
            end
          (* The instruction does not have a target. Do nothing. *)
          | (true, NONE, _) => (i, NONE)
          (* The instruction cannot be replaced. Do nothing. *)
          | (false, _, _) => (i, NONE)
    end


fun addValMoves1 vtn ins i key =
    case HashTable.lookup vtn key of
        (n, SOME dest) =>
        let
            val tgt = valOf (#2 (getST i))
        in
            print ("Added move from " ^ regToStr tgt ^ " to " ^ regToStr dest ^ "\n");
            ins @ [i, INS_RR {opcode=OP_MOV, r1=tgt, dest=dest}]
        end
      | (n, NONE) => ins @ [i]


fun addValMoves vtn ((i, replOpt), ins) =
    case replOpt of SOME key => addValMoves1 vtn ins i key | NONE => ins @ [i]


(* TODO: Debugging functions, remove later. *)
(* fun optToStr regOpt = case regOpt of NONE => "NONE" | SOME r => "SOME " ^ iToS r *)
(* fun printPair (expr, (number, regOpt)) = *)
(*     print (expr ^ " -> (" ^ iToS number ^ ", " ^ optToStr regOpt ^ ")\n") *)
(* fun printVtn id vtn = app printPair (HashTable.listItemsi vtn) *)


fun optBB ii node =
    let
        val (id, ins) = Cfg.getData node
        val _ = print ("/----BB" ^ id ^ "----\\\n");
        val vtn = mkHt () (* Value-to-Number map *)
        val nextNum = ref 0
        val newIns = map (numberVals ii nextNum vtn) ins
        val newIns = foldl (addValMoves vtn) [] newIns
        fun dash s n = if n = 0 then s else dash (s ^ "-") (n - 1)
    in
        (* printVtn id vtn; *)
        (id, newIns) before
        print ("\\" ^ dash "" ((size id) + 10) ^ "/\n")
    end


fun optFunc (ii as II {id=id, cfg=cfg, ...}) =
    (Cfg.app (optBB ii) cfg; (id, cfg))


fun lvNumbering iis = map optFunc iis

end
