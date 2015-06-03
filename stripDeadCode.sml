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

fun getTOpt i = #2 (getST i)
fun hasNewerDef reg gen = exists (fn (def, _, _) => def = reg) gen
fun notInvolved tgt (reg, _, _) = tgt <> reg


(* If the instruction is the last definition in the basic block, then add
 * the definition formed from the target of the instruction, the label
 * of the basic block, and the index position of the instruction to the gen
 * set. Then add to the kill set all definitions which match the target
 * register *)
fun iToGK defs id (i, (gen, kill, n)) =
    case getTOpt i of
        NONE =>
        (gen, kill, n -1)
      | SOME tgt =>
        (if not (hasNewerDef tgt gen) then add (gen, (tgt, id, n)) else gen,
         union (kill, filter (notInvolved tgt) defs),
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


(* TODO: Remove these later! *)
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


fun buildDFAs cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app propagate cfg; buildDFAs cfg)
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
    ((if n = pos andalso not mark
      then (i, true) before iRef := (true, i)
      else (i, mark))::ins,
     n + 1)


fun markDefIns iRef pos ins =
    #1 (List.foldr (markDefIns1 iRef pos) ([], 0) ins)


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

(* The ref will hold the instruction that corresp. to the def. The bool
 * in the pair indicated whether the ref was modified. If it was, then
 * send it down to be included in the worklist. Otherwise toss it. The
 * ref will not be changed if the mark is already set. *)
fun getDefI cfg (def, defIns) =
    let
        val iRef = ref (false, INS_X {opcode=OP_RET})
    in
        Cfg.app (markDefNode iRef def) cfg;
        if #1 (!iRef) then add (defIns, #2 (!iRef)) else defIns
    end


fun getDefs reaches (source, accum) =
    union (accum, filter (fn (reg, _, _) => reg = source) reaches)


fun getBefore1 _ _ [] = []
  | getBefore1 doAdd i ((x, mark)::xs) =
    if x = i then getBefore1 false i xs
    else if doAdd then [(x, mark)] @ getBefore1 doAdd i xs
    else getBefore1 doAdd i xs


fun getBefore ins i = getBefore1 true i ins


fun reachesLocal1 id ((i, _), (reaches, n)) =
    case getTOpt i of
        NONE => (reaches, n + 1)
      | SOME tgt =>
        (add (filter (notInvolved tgt) reaches, (tgt, id, n)), n + 1)


fun reachesLocal reaches id ins i =
    #1 (List.foldl (reachesLocal1 id) (reaches, 0) (getBefore ins i))


fun getLocalDefI1 pos ((i, mark), (ins, n)) =
    (if n = pos andalso not mark then i::ins else ins, n + 1)


fun getLocalDefI ins ((_, _, pos), defIns) =
    let
        val (ins, _) = List.foldl (getLocalDefI1 pos) ([], 0) ins
    in
        addList (defIns, ins)
    end


fun markLocalDef locals (i, mark) =
    if member (locals, i) then (i, true) else (i, mark)


fun mark1 cfg reaches work id ins =
    case pick work of
        NONE => ins
      | SOME (i, rest) =>
        let
            val (sources, _) = getST i
            (* defs will be a set of all the definitions of all the sources. *)
            val defs = foldr (getDefs (reachesLocal reaches id ins i)) (empty ())
                             (addList (empty (), sources))
            val _ = print ("source defs for " ^ insToStr i ^ ": [" ^ Util.foldd ", " defToStr (listItems defs) ^ "]\n")
            val awayDefs = filter (fn (_, defId, _) => defId <> id) defs
            val localDefs = difference (defs, awayDefs)
            val awayIns = foldr (getDefI cfg) (empty ()) awayDefs
            val localIns = foldr (getLocalDefI ins) (empty ()) localDefs
        in
            (*possible bug here? looks like ins never changes?? *)
            mark1 cfg reaches (union (rest, union (awayIns, localIns))) id
                  (List.map (markLocalDef localIns) ins)
        end


(* fun imToStr (i, mark) = "(" ^ insToStr i ^ ", " ^ Bool.toString mark ^ ")" *)

fun mark cfg node =
    let
        val DFA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
        val (ins, work) = List.foldr markCritical ([], empty ()) ins
        val _ = print (id ^ " critical: [" ^ Util.foldd ", " insToStr (listItems work) ^ "]\n")
        val ins = mark1 cfg reaches work id ins
    in
        (* The diff field doesn't matter now. *)
        DFA {id=id, ins=ins, gk=gk, reaches=reaches, diff=false}
    end


fun sweepIns ((i, mark), L) = (* if mark then i::L else L *)
    if mark then i::L else
    (print ("Removing instruction: " ^ insToStr i ^ "\n");
     L)


fun sweep (DFA {id=id, ins=ins, ...}) = (id, List.foldr sweepIns [] ins)


(* needs to add all defs possible to the list *)
fun findDefs1 id (i, (defs, n)) =
    (case getTOpt i of NONE => defs | SOME tgt => (tgt, id, n)::defs, n + 1)


fun findDefs ((id, ins), defs) =
    addList (defs, #1 (List.foldl (findDefs1 id) ([], 0) ins))


fun optFunc (id, cfg) =
    let
        val defs = Cfg.fold findDefs (empty ()) cfg
        val lvas = buildDFAs (Cfg.map (bbToDFA defs) cfg)
    in (*I actually think we're OK up to here...*)
        Cfg.app (mark lvas) lvas;
        (id, Cfg.map sweep lvas)
        before print "----------------------\n"
    end


fun stripDeadCode prog = List.map optFunc prog

end
