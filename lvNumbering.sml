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
fun replaceOK opc = not (has opc [OP_LOADRET, OP_NEW, OP_MOVEQ, OP_LOADGLOBAL])


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
                numberTarget next vtn tgt;
                print (insToStr i ^ " --> " ^ insToStr newI ^ "\n");
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
                numberTarget next vtn tgt;
                insert vtn (exprToStr opc nums, (n, SOME dest));
                print (insToStr i ^ " --> " ^ insToStr newI ^ "\n");
                (newI, NONE)
            end
          (* The instruction has a target, but a value goes not exist for it.
           * Insert one into the vtn. *)
          | (true, SOME tgt, NONE) =>
            let
                val n = numberTarget next vtn tgt
                val key = exprToStr opc nums
            in
                insert vtn (key, (n, NONE)); (i, SOME key)
            end
          (* The instruction cannot be replaced, but has a target. Kill the
           * target in the vtn but don't try to replace the instruction. *)
          | (false, SOME tgt, NONE) => (numberTarget next vtn tgt; (i, NONE))
          (* The instruction cannot be replaced. Do nothing. *)
          | (false, _, _) => (i, NONE)
          (* The instruction does not have a target. Do nothing. *)
          | (true, NONE, _) => (i, NONE)
    end


fun addValMoves1 vtn ins i key =
    case HashTable.lookup vtn key of
        (n, SOME dest) =>
        let
            val tgt = valOf (#2 (getST i))
            val newI = INS_RR {opcode=OP_MOV, r1=tgt, dest=dest}
        in
            print ("+ " ^ insToStr i ^ " --> " ^ insToStr i ^ ", " ^
                   insToStr newI ^ "\n");
            ins @ [i, newI]
        end
      | (n, NONE) => ins @ [i]


fun addValMoves vtn ((i, replOpt), ins) =
    case replOpt of SOME key => addValMoves1 vtn ins i key | NONE => ins @ [i]


fun optBB ii node =
    let
        val (id, ins) = Cfg.getData node
        val vtn = mkHt () (* Value-to-Number map *)
        val nextNum = ref 0
        fun dash s n = if n = 0 then s else dash (s ^ "-") (n - 1)
    in
        print ("/----BB" ^ id ^ "----\\\n");
        (id, foldl (addValMoves vtn) [] (map (numberVals ii nextNum vtn) ins))
        before print ("\\" ^ dash "" ((size id) + 10) ^ "/\n")
    end


fun optFunc (ii as II {id=id, cfg=cfg, ...}) =
    (Cfg.app (optBB ii) cfg; (id, cfg))


fun lvNumbering iis = map optFunc iis

end
