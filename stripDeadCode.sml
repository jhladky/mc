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
    let
        val (_, targets) = getST ins
        val condAdd = fn (tgt, gen) => if not (hasNewerDef tgt gen)
                                       then add (gen, (tgt, id, n))
                                       else gen
    in
        (* The kill set part is probably wrong! *)
        (List.foldr condAdd gen targets,
         List.foldr (fn (tgt, kill) => add (kill, (tgt, id, n))) kill targets,
         n - 1)
    end


fun bbToGK (id, ins) =
    let
        val (gen, kill, n) = List.foldr (insToGK id) (empty (), empty (),
                                                      length ins - 1) ins
        val ins = List.map (fn i => (i, true)) ins
    in
        DFA {id=id, ins=ins, gk=(gen, kill), reaches=empty (), diff=true}
    end


fun diffCheck (DFA {diff=d, ...}, diff) = if d then true else diff


fun bbReaches1 (DFA {gk=(gen, kill), reaches=reaches, ...}, s) =
    union (s, union (gen, difference (reaches, kill)))


(* Reaches(n) = the union of each basic block m for all m in pred(n),
 * where n is the current basic block OF gen(m) U (Reaches(m) - kill(m)) *)
fun bbReaches node =
    let
        val DFA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
        val rs = List.foldr bbReaches1 (empty ()) (Cfg.getPreds node)
        val diff = not (equal (reaches, rs))
    in
        DFA {id=id, ins=ins, gk=gk, reaches=rs, diff=diff}
    end


fun buildLvas cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app bbReaches cfg; buildLvas cfg)
    else cfg


(* -> INS_R is OP_PRINT, OP_PRINTLN, OP_READ, OP_STORERET, OP_LOADRET, OP_DEL
 * -> INS_L is OP_CALL, OP_JUMPI
 * -> INS_CLL is OP_CBREQ
 * -> INS_RIC is OP_COMPI
 * -> INS_RRC is OP_COMP
 * -> INS_NEW is OP_NEW
 * -> INS_SR is OP_COMPUTEGLOBALADDRESS, OP_LOADGLOBAL
 * -> INS_RS is OP_STOREGLOBAL
 * -> INS_X is OP_RET
 * -> INS_RRI is OP_STOREAI *)
val isCritical =
 fn INS_R {...}                     => true
  | INS_L {...}                     => true
  | INS_X {...}                     => true
  | INS_CLL {...}                   => true
  | INS_RIC {...}                   => true
  | INS_RRC {...}                   => true
  | INS_NEW {...}                   => true
  | INS_SR {...}                    => true
  | INS_RS {...}                    => true
  | INS_RRI {...}                   => true
  | INS_RIR {opcode=OP_LOADAI, ...} => true
  | _                               => false


fun markCritical ((i, _), (ins, work)) =
    if isCritical i then ((i, true)::ins, add (work, i))
    else ((i, false)::ins, work)

(* mark () { *)
(*      worklist <- 0 *)
(*      for each instruction i *)
(*          clear i's mark *)
(*          if i is critical: *)
(*             mark i *)
(*             worklist <= worklist U {i} *)

(*      while worklist <> 0: *)
(*            remove i from worklist *)
(*            for each s in source(i) *)
(*                for each d in defs(s): *)
(*                    if d is not marked: *)
(*                       mark d *)
(*                       worklist <- worklist U {d} *)

fun getDefs reaches source = filter (fn (reg, _, _) => reg = source) reaches


fun mark1 reaches work ins =
    case pick work of
        NONE => ins
      | SOME ((i, mark), rest) =>
        let
            val (sources, _) = getST i
            val defs = List.foldr (getDefs reaches) [] sources
        in
            markDefs defs; (* HERE'S THE TRICKY STEP! *)
            mark1 reaches (addList (work, defs)) ins
        end


fun mark node =
    let
        val DFA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
        val (ins, work) = List.foldr markCritical ([], empty ()) ins
        val ins = mark1 reaches work ins
    in
        DFA {id=id, ins=ins, gk=gk, reaches=reaches, diff=false}
    end


fun sweepIns ((i, mark), L) = if mark then i::L else L


fun sweep (DFA {id=id, ins=ins, ...}) = (id, List.foldr sweepIns [] ins)


fun optFunc (id, cfg) =
    let
        val lvas = buildLvas (Cfg.map bbToGK cfg)
    in
        Cfg.app mark lvas;
        (id, Cfg.map sweep lvas)
    end


fun stripDeadCode prog = List.map optFunc prog

end
