signature STRIP_DEAD_CODE = sig
    val stripDeadCode : Iloc.program -> Iloc.program
end

structure StripDeadCode :> STRIP_DEAD_CODE = struct
open Iloc
open UnorderedSet

type definition = int * string * int

datatype dataflow_analysis =
         DFA of {
             id: string,
             ins: (instruction * bool) list,
             gk: definition set * definition set,
             reaches: definition set,
             diff: bool
         }

fun hasNewerDef reg gen = exists (fn (def, _, _) => def = reg) gen


(* If the instruction is the last definition in the basic block, then add
 * the definition formed from the target of the instruction, the label
 * of the basic block, and the index position of the instruction to the gen
 * set. Then add to the kill set all definitions which match the target
 * register *)
fun iToGK defs id (ins, (gen, kill, n)) =
    case #2 (getST ins) of
        NONE =>
        (gen, kill, n -1)
      | SOME tgt =>
        (if not (hasNewerDef tgt gen) then add (gen, (tgt, id, n)) else gen,
         union (kill, filter (fn (reg, _, _) => reg <> tgt) defs),
         n - 1)


fun insToGK defs (id, ins) =
    List.foldr (iToGK defs id) (empty (), empty (), length ins - 1) ins


fun bbToDFA defs (id, ins) =
    let
        val (gen, kill, _) = insToGK defs (id, ins)
        val ins = List.map (fn i => (i, true)) ins
    in
        DFA {id=id, ins=ins, gk=(gen, kill), reaches=empty (), diff=true}
    end


fun diffCheck (DFA {diff=d, ...}, diff) = if d then true else diff


fun propagate1 (DFA {gk=(gen, kill), reaches=reaches, ...}, s) =
    union (s, union (gen, difference (reaches, kill)))


(* Reaches(n) = the union of each basic block m for all m in pred(n),
 * where n is the current basic block OF gen(m) U (Reaches(m) - kill(m)) *)
fun propagate node =
    let
        val DFA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
        val rs = List.foldr propagate1 (empty ()) (Cfg.getPreds node)
        val diff = not (equal (reaches, rs))
    in
        DFA {id=id, ins=ins, gk=gk, reaches=rs, diff=diff}
    end



fun defToStr (reg, label, position) =
    "(" ^ rToStr reg ^ " " ^ label ^ " " ^ Util.iToS position ^ ")"


(* TODO: Remove these later! *)
fun printReaches node =
    let
        val dfa as DFA {id=id, reaches=reaches, ...} = Cfg.getData node
    in
        print (id ^ " reaches: [" ^ Util.foldd ", " defToStr (listItems reaches) ^ "]\n");
        dfa
    end


fun buildLvas cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app propagate cfg; buildLvas cfg)
    else (Cfg.app printReaches cfg; cfg)


fun isCriticalRR opc =
    Util.has opc [OP_MOVEQ, OP_MOVNE, OP_MOVLT, OP_MOVGT, OP_MOVLE, OP_MOVGE]


(* -> INS_R is OP_PRINT, OP_PRINTLN, OP_READ, OP_STORERET, OP_LOADRET, OP_DEL
 * -> INS_L is OP_CALL, OP_JUMPI
 * -> INS_CLL is OP_CBREQ
 * -> INS_RIC is OP_COMPI
 * -> INS_RI is OP_STOREOUTARGUMENT
 * -> INS_RRC is OP_COMP
 * -> INS_NEW is OP_NEW (* possibly not necessary *)
 * -> INS_SR is OP_COMPUTEGLOBALADDRESS, OP_LOADGLOBAL
 * -> INS_RS is OP_STOREGLOBAL
 * -> INS_X is OP_RET
 * -> INS_RRI is OP_STOREAI
 * -> INS_RR is OP_MOVEQ, OP_MOVNE, OP_MOVLT, OP_MOVGT, OP_MOVLE, OP_MOVGE*)
val isCritical =
 fn INS_R {...}                     => true
  | INS_L {...}                     => true
  | INS_X {...}                     => true
  | INS_CLL {...}                   => true
  | INS_RIC {...}                   => true
  | INS_RI {...}                    => true
  | INS_RRC {...}                   => true
  | INS_NEW {...}                   => true
  | INS_SR {...}                    => true
  | INS_RS {...}                    => true
  | INS_RRI {...}                   => true
  | INS_RIR {opcode=OP_LOADAI, ...} => true
  | INS_RR {opcode=opc, ...}        => isCriticalRR opc
  | _                               => false


fun markCritical ((i, _), (ins, work)) =
    if isCritical i then ((i, true)::ins, add (work, i))
    else ((i, false)::ins, work)


fun markDefIns1 iRef pos ((i, mark), (ins, n)) =
    ((if n = pos
      then (i, true) before iRef := i
      else (i, mark))::ins,
     n + 1)


fun markDefIns iRef pos ins =
    let
        val (ins, _) = List.foldr (markDefIns1 iRef pos) ([], 0) ins
    in
        ins
    end


fun markDefNode iRef (_, defId, pos) node =
    let
        val dfa as DFA {id=id, ins=ins, gk=gk, reaches=rs, ...} =
            Cfg.getData node
    in
        if defId <> id then dfa
        else DFA {
                id=id,
                ins=markDefIns iRef pos ins,
                gk=gk,
                reaches=rs,
                diff=false
            }
    end


fun getDefI cfg def =
    let
        val iRef = ref (INS_X {opcode=OP_RET}) (* Dummy instruction. *)
    in
        Cfg.app (markDefNode iRef def) cfg;
        (!iRef)
    end


(* --- Strip Dead Code version of the local updating ---
 * The reaches set needs to be updated at every instructions.
 * The target of an instruction kills any previous instruction with the same target.
 * This definition is then added to the reaches set for the instructions that follow.
 * In terms of ordering, we take the current reaches set and use it to get the definitions
 * for the SOURCES of the instruction, and THEN when we pass reaches down, we update it.
 * As usual this reaches we create we just toss when we're done.
 * NO! mark1 is not ITERATING through the instructions!. It is iterating through the workList.
 * So we have to update reaches on a per-instruction basis with the local block info.
 * And it has to be computed then and there every time, per instruction.
 * Computing it:
 * 1. Get rid of all instructions under and including that instruction.
 * 2. Calculate the reaching defs for that little block.
 * 3. Add that to the reaching we already have, making sure to remove the defs that are killed
 *    previously in reaching. *)

fun getDefs reaches (source, accum) =
    union (accum, filter (fn (reg, _, _) => reg = source) reaches)


fun getBefore1 _ _ [] = []
  | getBefore1 doAdd i (x::xs) =
    if x = i then getBefore1 false i xs
    else if doAdd then [x] @ getBefore1 doAdd i xs
    else getBefore1 doAdd i xs


fun getBefore i ins = getBefore1 true i ins


(* how are we going to do this???? *)

(*how calculate the reaching defs for there instructions*)
(*split ins into two list, before i, and after and including i*)
fun reachesLocal reaches ins i =
    let
        val ins = getBefore i ins
    (*does having the id matter in this case??? I don't *think* it does.... but it might
         * I think we might need to pass it in. *)
        (* val (gen, kill, _) = insToGK ("", ins) *)

    in
        reaches
    end


fun mark1 cfg reaches work ins =
    case pick work of
        NONE => ins
      | SOME (i, rest) =>
        let
            val (sources, _) = getST i
            (* defs will be a set of all the definitions of all the sources. *)
            val defs = foldr (getDefs reaches) (empty ())
                             (addList (empty (), sources))
            val _ = print ("source defs: [" ^ Util.foldd ", " defToStr (listItems defs) ^ "]\n")
            val defIns = map (getDefI cfg) defs
        in
            mark1 cfg reaches (union (rest, defIns)) ins
        end


(* fun imToStr (i, mark) = "(" ^ insToStr i ^ ", " ^ Bool.toString mark ^ ")" *)

fun mark cfg node =
    let
        val DFA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
        val (ins, work) = List.foldr markCritical ([], empty ()) ins
        val _ = print (id ^ " critical: [" ^ Util.foldd ", " insToStr (listItems work) ^ "]\n")
        val ins = mark1 cfg reaches work ins
    in
        DFA {id=id, ins=ins, gk=gk, reaches=reaches, diff=false}
    end


fun sweepIns ((i, mark), L) = (* if mark then i::L else L *)
    if mark then i::L else
    (print ("Removing instruction: " ^ insToStr i ^ "\n");
     L)


fun sweep (DFA {id=id, ins=ins, ...}) = (id, List.foldr sweepIns [] ins)


(*needs to add all defs possible to the list*)
fun findDefs1 id (i, (defs, n)) =
    (case #2 (getST i) of NONE => defs | SOME tgt => (tgt, id, n)::defs, n + 1)


fun findDefs ((id, ins), defs) =
    addList (defs, #1 (List.foldl (findDefs1 id) ([], 0) ins))


fun optFunc (id, cfg) =
    let
        val defs = Cfg.fold findDefs (empty ()) cfg
        val lvas = buildLvas (Cfg.map (bbToDFA defs) cfg)
    in
        Cfg.app (mark lvas) lvas;
        (id, Cfg.map sweep lvas)
        before print "----------------------\n"
    end


fun stripDeadCode prog = List.map optFunc prog

end
