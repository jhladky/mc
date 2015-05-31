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



fun hasNewerDef reg genSet = exists (fn (def, _, _) => def = reg) genSet


(* gen(n) = the defs in n of r for which the defined name (r) is not redefined later.
 * In a given block it is just the last definition of the register.
 * kill(n) = All the definitions of r that are killed in n only.
 * (the register in the definition is a target of the an instruction in n) *)
fun insToGK id (ins, (gen, kill, n)) =
    (* The kill set part is probably wrong! *)
    case #2 (getST ins) of
        NONE =>
        (gen, kill, n -1)
      | SOME tgt =>
        (if not (hasNewerDef tgt gen) then add (gen, (tgt, id, n)) else gen,
         add (kill, (tgt, id, n)),
         n - 1)


fun bbToGK (id, ins) =
    let
        val (gen, kill, n) = List.foldr (insToGK id) (empty (), empty (),
                                                      length ins - 1) ins
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


fun buildLvas cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app propagate cfg; buildLvas cfg)
    else cfg


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


fun getDefs reaches (source, accum) =
    union (accum, filter (fn (reg, _, _) => reg = source) reaches)


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
        else DFA {id=id, ins=markDefIns iRef pos ins, gk=gk, reaches=rs, diff=false}
    end


fun getDefI cfg def =
    let
        val iRef = ref (INS_X {opcode=OP_RET}) (* Dummy instruction. *)
    in
        Cfg.app (markDefNode iRef def) cfg;
        (!iRef)
    end


fun mark1 cfg reaches work ins =
    case pick work of
        NONE => ins
      | SOME (i, rest) =>
        let
            val (sources, _) = getST i
            val defs = foldr (getDefs reaches) (empty ())
                             (addList (empty (), sources))
            val defIns = map (getDefI cfg) defs
        in
            mark1 cfg reaches (union (rest, defIns)) ins
        end


fun mark cfg node =
    let
        val DFA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
        val (ins, work) = List.foldr markCritical ([], empty ()) ins
        val ins = mark1 cfg reaches work ins
    in
        DFA {id=id, ins=ins, gk=gk, reaches=reaches, diff=false}
    end


fun sweepIns ((i, mark), L) = (* if mark then i::L else L *)
    if mark then i::L else
    (print ("Removing instruction: " ^ insToStr i ^ "\n");
     L)


fun sweep (DFA {id=id, ins=ins, ...}) = (id, List.foldr sweepIns [] ins)


fun optFunc (id, cfg) =
    let
        val lvas = buildLvas (Cfg.map bbToGK cfg)
    in
        Cfg.app (mark lvas) lvas;
        (id, Cfg.map sweep lvas) before print "----------------------\n"
    end


fun stripDeadCode prog = List.map optFunc prog

end
