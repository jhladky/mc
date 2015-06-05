signature STRIP_DEAD_CODE = sig
    val stripDeadCode : Iloc.program -> Iloc.program
end

structure StripDeadCode :> STRIP_DEAD_CODE = struct
open Util
open Dfa
open Iloc
open UnorderedSet

type definition = int * string * int


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
        DFA {id=id, ins=ins, gk=(gen, kill), propSet=empty (), diff=true}
    end


fun removeKilled reaches kill =
    filter (fn (tgt, _, _) => not (exists (involved tgt) kill)) reaches


fun propagate1 (DFA {gk=(gen, kill), propSet=reaches, ...}, s) =
    union (s, union (gen, removeKilled reaches kill))


fun propagate node =
    let
        val DFA {id=id, ins=ins, gk=gk, propSet=reaches, ...} = Cfg.getData node
        val rs = List.foldr propagate1 (empty ()) (Cfg.getPreds node)
        val diff = not (equal (reaches, rs))
    in
        DFA {id=id, ins=ins, gk=gk, propSet=rs, diff=diff}
    end


fun isCriticalRR opc =
    has opc [OP_MOVEQ, OP_MOVNE, OP_MOVLT, OP_MOVGT, OP_MOVLE, OP_MOVGE]


(* INS_R:   {OP_PRINT, OP_PRINTLN, OP_READ, OP_STORERET, OP_LOADRET, OP_DEL}
 * INS_L:   {OP_CALL, OP_JUMPI}
 * INS_CLL: {OP_CBREQ}
 * INS_SR:  {OP_COMPUTEGLOBALADDRESS, OP_LOADGLOBAL}
 * INS_RR:  {OP_MOVEQ, OP_MOVNE, OP_MOVLT, OP_MOVGT, OP_MOVLE, OP_MOVGE} *)
val isCritical =
 fn INS_R {...}                              => true
  | INS_L {...}                              => true
  | INS_X {opcode=OP_RET, ...}               => true
  | INS_CLL {...}                            => true
  | INS_RIC {opcode=OP_COMPI, ...}           => true
  | INS_RI {opcode=OP_STOREOUTARGUMENT, ...} => true
  | INS_RRC {opcode=OP_COMP, ...}            => true
  | INS_SR {...}                             => true
  | INS_RS {opcode=OP_STOREGLOBAL, ...}      => true
  | INS_RRI {opcode=OP_STOREAI, ...}         => true
  | INS_RIR {opcode=OP_LOADAI, ...}          => true
  | INS_RR {opcode=opc, ...}                 => isCriticalRR opc
  | _                                        => false


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
        val dfa as DFA {id=id, ins=ins, gk=gk, propSet=rs, ...} =
            Cfg.getData node
    in
        if defId = id
        then DFA {id=id, ins=mapn (markI pos) ins, gk=gk,
                  propSet=rs, diff=false}
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
        val DFA {propSet=reaches, ins=ins, ...} =
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
    if mark then i::L else L before print ("- " ^ insToStr i ^ "\n")


fun sweep (DFA {id=id, ins=ins, ...}) = (id, List.foldr sweepIns [] ins)


fun findDefs1 id (i, (defs, n)) =
    (case getTOpt i of NONE => defs | SOME tgt => (tgt, id, n)::defs, n + 1)


fun findDefs ((id, ins), defs) =
    addList (defs, #1 (List.foldl (findDefs1 id) ([], 0) ins))


fun optFunc (id, cfg) =
    let
        val defs = Cfg.fold findDefs (empty ()) cfg
        val dfas = buildDFAs propagate (Cfg.map (bbToDFA defs) cfg)
        fun dash s n = if n = 0 then s else dash (s ^ "-") (n - 1)
    in
        print ("/-----" ^ id ^ "-----\\\n");
        mark dfas;
        (id, Cfg.map sweep dfas)
        before print ("\\" ^ dash "" ((size id) + 10) ^ "/\n")
    end


fun stripDeadCode prog = List.map optFunc prog

end
