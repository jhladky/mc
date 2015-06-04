signature STRIP_DEAD_CODE = sig
    val stripDeadCode : Iloc.program -> Iloc.program
end

structure StripDeadCode :> STRIP_DEAD_CODE = struct
open Util
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
fun involved tgt (reg, _, _) = tgt = reg


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
         union (kill, filter (involved tgt) defs),
         n - 1)


fun insToGK defs (id, ins) =
    List.foldr (iToGK defs id) (empty (), empty (), length ins - 1) ins


(* TODO: Remove these later! *)
fun defToStr (reg, label, position) =
    "(" ^ rToStr reg ^ " " ^ label ^ " " ^ iToS position ^ ")"


fun bbToDFA defs (id, ins) =
    let
        val (gen, kill, _) = insToGK defs (id, ins)
        val ins = List.map (fn i => (i, false)) ins
    in
        (* print (id ^ " gen: " ^ toString defToStr gen ^ "\n"); *)
        (* print (id ^ " kill: " ^ toString defToStr kill ^ "\n"); *)
        DFA {id=id, ins=ins, gk=(gen, kill), reaches=empty (), diff=true}
    end


fun diffCheck (DFA {diff=d, ...}, diff) = if d then true else diff


fun notInKill tgt1 kill = not (exists (fn (tgt2, _, _) => tgt1 = tgt2) kill)


fun removeKilled reaches kill =
    filter (fn (tgt, _, _) => notInKill tgt kill) reaches


fun propagate1 (DFA {gk=(gen, kill), reaches=reaches, ...}, s) =
    union (s, union (gen, removeKilled reaches kill))


(*There is a problem here!*)

(* Reaches(n) = the union of each basic block m for all m in pred(n),
 * where n is the current basic block OF gen(m) U (Reaches(m) - kill(m)) *)
fun propagate node =
    let
        val DFA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
        val rs = List.foldr propagate1 (empty ()) (Cfg.getPreds node)
        val diff = not (equal (reaches, rs))
    in
        (* print (id ^ " tmp reaches: " ^ toString defToStr reaches ^ "\n"); *)
        (* print (id ^ " tmp rs:      " ^ toString defToStr rs ^ "\n"); *)
        (* print (id ^ " diff:        " ^ Bool.toString diff ^ "\n"); *)
        DFA {id=id, ins=ins, gk=gk, reaches=rs, diff=diff}
    end


(* TODO: Remove these later! *)
fun printReaches node =
    let
        val dfa as DFA {id=id, reaches=reaches, ...} = Cfg.getData node
    in
        print (id ^ " reaches: " ^ toString defToStr reaches ^ "\n"); dfa
    end


fun buildDFAs cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app propagate cfg; buildDFAs cfg)
    (* else (Cfg.app printReaches cfg; cfg) *)
    else cfg


fun isCriticalRR opc =
    has opc [OP_MOVEQ, OP_MOVNE, OP_MOVLT, OP_MOVGT, OP_MOVLE, OP_MOVGE]


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


fun getCriticalIns id ((i, _), (critical, n)) =
    (if isCritical i then add (critical, (i, id, n)) else critical,
     n + 1)


fun getCritical (DFA {id=id, ins=ins, ...}, accum) =
    let
        val (critical, _) = List.foldl (getCriticalIns id) (empty (), 0) ins
    in
        union (accum, critical)
    end


(* TODO: Debugging function. Remove later. *)
fun wToStr (i, id, pos) = "<" ^ insToStr i ^ ", " ^ id ^ ", " ^ iToS pos ^ ">"


fun markI pos (i, mark) n = if pos = n then (i, true) (*before print ("marking " ^ wToStr (i, "_", pos) ^ "\n") *)else (i, mark)


fun markIns1 (_, defId, pos) node =
    let
        val dfa as DFA {id=id, ins=ins, gk=gk, reaches=rs, ...} =
            Cfg.getData node
    in
        if defId = id
        then DFA {id=id, ins=mapn (markI pos) ins, gk=gk,
                  reaches=rs, diff=false}
        else dfa
    end


fun markIns cfg def = Cfg.app (markIns1 def) cfg


fun getDfa cfg defId = valOf (Cfg.find (fn DFA {id=id, ...} => defId = id) cfg)


fun getToMarkI id pos ((i, mark), (toMark, n)) =
    (if n = pos andalso not mark then add (toMark, (i, id, pos)) else toMark,
     n + 1)


fun getToMark1 (_, defId, pos) (DFA {id=id, ins=ins, ...}, toMark) =
    if defId = id
    then union (toMark, #1 (List.foldl (getToMarkI id pos) (empty (), 0) ins))
    else toMark


fun getToMark cfg (def, toMark) =
    union (toMark, Cfg.fold (getToMark1 def) (empty ()) cfg)


(* return a list of instructions to mark *)
fun mark1 cfg (i, id, pos) =
    let
        val (sources, _) = getST i
        val DFA {reaches=reaches, ins=ins, ...} = getDfa cfg id
        val defs = fold (getDefs (reachesLocal reaches id ins i)) (empty ())
                        (addList (empty (), sources))
        val toMark = fold (getToMark cfg) (empty ()) defs
        (* val _ = print (insToStr i ^ " source defs: " ^ toString defToStr defs ^ ", ") *)
        (* val _ = print ("toMark: " ^ toString wToStr toMark ^ "\n") *)
    in
        toMark
    end


fun mark work cfg =
    case pick work of
        NONE => ()
      | SOME (i, rest) =>
        let
            val toMark = mark1 cfg i
        in
            app (markIns cfg) toMark;
            mark (union (rest, toMark)) cfg
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
        val _ = print ("/-----" ^ id ^ "-----\\\n");
        val defs = Cfg.fold findDefs (empty ()) cfg
        val lvas = buildDFAs (Cfg.map (bbToDFA defs) cfg)
        (* I actually think we're OK up to this line. *)

        (*merge this back in at some point*)
        val critical = Cfg.fold getCritical (empty ()) lvas
        (* val _ = print ("critical: " ^ toString wToStr critical ^ "\n") *)
        fun dash s n = if n = 0 then s else dash (s ^ "-") (n - 1)
    in
        app (markIns lvas) critical;
        mark critical lvas;
        (id, Cfg.map sweep lvas) before
        print ("\\" ^ dash "" ((size id) + 10) ^ "/\n")
    end


fun stripDeadCode prog = List.map optFunc prog

end
