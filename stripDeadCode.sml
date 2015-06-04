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
fun involved tgt (reg, _, _) = tgt = reg
fun getSourceSet i = addList (empty (), #1 (getST (#1 i)))


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


fun bbToDFA defs (id, ins) =
    let
        val (gen, kill, _) = insToGK defs (id, ins)
        val ins = List.map (fn i => (i, false)) ins
    in
        DFA {id=id, ins=ins, gk=(gen, kill), reaches=empty (), diff=true}
    end


fun diffCheck (DFA {diff=d, ...}, diff) = if d then true else diff


fun notInKill tgt1 kill = not (exists (fn (tgt2, _, _) => tgt1 = tgt2) kill)


fun removeKilled reaches kill =
    filter (fn (tgt, _, _) => notInKill tgt kill) reaches


fun propagate1 (DFA {gk=(gen, kill), reaches=reaches, ...}, s) =
    union (s, union (gen, removeKilled reaches kill))


fun propagate node =
    let
        val DFA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
        val rs = List.foldr propagate1 (empty ()) (Cfg.getPreds node)
        val diff = not (equal (reaches, rs))
    in
        DFA {id=id, ins=ins, gk=gk, reaches=rs, diff=diff}
    end


fun buildDFAs cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app propagate cfg; buildDFAs cfg)
    else cfg


fun isCriticalRR opc =
    has opc [OP_MOVEQ, OP_MOVNE, OP_MOVLT, OP_MOVGT, OP_MOVLE, OP_MOVGE]


(* -> INS_R is OP_PRINT, OP_PRINTLN, OP_READ, OP_STORERET, OP_LOADRET, OP_DEL
 * -> INS_L is OP_CALL, OP_JUMPI
 * -> INS_CLL is OP_CBREQ
 * -> INS_RIC is OP_COMPI
 * -> INS_RI is OP_STOREOUTARGUMENT
 * -> INS_RRC is OP_COMP
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
  | INS_SR {...}                    => true
  | INS_RS {...}                    => true
  | INS_RRI {...}                   => true
  | INS_RIR {opcode=OP_LOADAI, ...} => true
  | INS_RR {opcode=opc, ...}        => isCriticalRR opc
  | _                               => false


fun getCriticalIns id ((i, _), (critical, n)) =
    (if isCritical i then add (critical, (i, id, n)) else critical, n + 1)


fun getCritical (DFA {id=id, ins=ins, ...}, accum) =
    let
        val (critical, _) = foldl (getCriticalIns id) (empty (), 0) ins
    in
        union (accum, critical)
    end


fun markI pos (i, mark) n = if pos = n then (i, true) else (i, mark)


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


fun getToMarkI id pos ((i, mark), (toMark, n)) =
    (if n = pos andalso not mark then add (toMark, (i, id, pos)) else toMark,
     n + 1)


fun getToMark1 (_, defId, pos) (DFA {id=id, ins=ins, ...}, toMark) =
    if defId = id
    then union (toMark, #1 (List.foldl (getToMarkI id pos) (empty (), 0) ins))
    else toMark


fun getToMark cfg (def, toMark) =
    union (toMark, Cfg.fold (getToMark1 def) (empty ()) cfg)


fun addLocalDef id ((i, _), (reaches, n)) =
    case getTOpt i of
        NONE => (reaches, n + 1)
      | SOME tgt =>
        (add (filter (not o involved tgt) reaches, (tgt, id, n)), n + 1)


fun getDefs cfg (i, id, pos) (source, accum) =
    let
        val DFA {reaches=reaches, ins=ins, ...} =
            valOf (Cfg.find (fn DFA {id=id2, ...} => id = id2) cfg)
        val reaches = #1 (foldl (addLocalDef id) (reaches, 0)
                                (List.take (ins, pos)))
    in
        union (accum, filter (fn (reg, _, _) => reg = source) reaches)
    end


fun mark1 work cfg =
    case pick work of
        NONE => ()
      | SOME (i, rest) =>
        let
            val defs = fold (getDefs cfg i) (empty ()) (getSourceSet i)
            val toMark = fold (getToMark cfg) (empty ()) defs
        in
            app (markIns cfg) toMark; mark1 (union (rest, toMark)) cfg
        end


fun mark cfg =
    let
        val critical = Cfg.fold getCritical (empty ()) cfg
    in
        app (markIns cfg) critical; mark1 critical cfg
    end


fun sweepIns ((i, mark), L) = (* if mark then i::L else L *)
    if mark
    then i::L
    else L before print ("Removing instruction: " ^ insToStr i ^ "\n")


fun sweep (DFA {id=id, ins=ins, ...}) = (id, List.foldr sweepIns [] ins)


fun findDefs1 id (i, (defs, n)) =
    (case getTOpt i of NONE => defs | SOME tgt => (tgt, id, n)::defs, n + 1)


fun findDefs ((id, ins), defs) =
    addList (defs, #1 (List.foldl (findDefs1 id) ([], 0) ins))


fun optFunc (id, cfg) =
    let
        val defs = Cfg.fold findDefs (empty ()) cfg
        val lvas = buildDFAs (Cfg.map (bbToDFA defs) cfg)
        fun dash s n = if n = 0 then s else dash (s ^ "-") (n - 1)
    in
        print ("/-----" ^ id ^ "-----\\\n");
        mark lvas;
        (id, Cfg.map sweep lvas) before
        print ("\\" ^ dash "" ((size id) + 10) ^ "/\n")
    end


fun stripDeadCode prog = List.map optFunc prog

end
