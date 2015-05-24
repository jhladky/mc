signature COPY_PROP = sig
    val copyProp : Iloc.program -> Iloc.program
end

structure CopyProp :> COPY_PROP = struct
open Iloc
open UnorderedSet

type copy = int * int

datatype live_variable_analysis =
         LVA of {
             id: string,
             ins: instruction list,
             gk: copy set * copy set,
             copyIn: copy set,
             ciDiff: bool
         }


fun has thing L = List.exists (fn t => t = thing) L


local
    fun has (LVA {id=lvaId, ...}) L =
        List.exists (fn LVA {id=id, ...} => id = lvaId) L


    fun mkPredRep1 reps (lva, _) =
        (lva, List.map #1 (List.filter (fn (_, succs) => has lva succs) reps))

in
    fun mkPredRep reps = List.map (mkPredRep1 reps) reps
end


local
    fun findCopies1 (INS_RR {opcode=OP_MOV, r1=r1, dest=dest}, copies) =
        (r1, dest)::copies
      | findCopies1 (ins, copies) = copies
in
    fun findCopies ((id, ins), cps) = cps @ List.foldr findCopies1 [] ins
end


(* If an instruction IS a copy and HAS NOT been killed add it to the gen set. *)
(* If the register target of an instruction HAS been involved in a copy
 * then add it to the kill set. *)
fun insToGK copies (gen, kill) (INS_RR {opcode=OP_MOV, r1=r1, dest=d}) =
    let
        val c = (r1, d)
        val gen = if not (member (kill, c)) then add (gen, c) else gen
    in
        (gen, add (kill, c))
    end
  | insToGK copies (gen, kill) ins =
    let
        (* The instruction IS NOT a copy so we add nothing to the gen set. *)
        val (_, targets) = getST ins
        val copies = List.filter (fn (src, tgt) => has src targets orelse
                                                   has tgt targets) copies
    in
        (gen, addList (kill, copies))
    end


fun bbToGK copies (id, ins) =
    LVA {
        id=id,
        ins=ins,
        gk=List.foldr (fn (ins, gk) => insToGK copies gk ins)
                      (empty (), empty ()) ins,
        copyIn=empty (),
        ciDiff=true
    }

val replaceIns =
 fn f =>
    (fn ins as INS_RRR {opcode=opc, dest=d, r1=r1, r2=r2} =>
        INS_RRR {opcode=opc, dest=f ins d, r1=f ins r1, r2=f ins r2}
      | ins as INS_RIR {opcode=opc, dest=d, r1=r1, immed=i} =>
        INS_RIR {opcode=opc, dest=d, r1=f ins r1, immed=i}
      | ins as INS_RRI {opcode=opc, r1=r1, r2=r2, immed=i} =>
        INS_RRI {opcode=opc, r1=f ins r1, r2=f ins r2, immed=i}
      | ins as INS_RRC {opcode=opc, r1=r1, r2=r2} =>
        INS_RRC {opcode=opc, r1=f ins r1, r2=f ins r2}
      | ins as INS_RIC {opcode=opc, r1=r1, immed=i} =>
        INS_RIC {opcode=opc, r1=f ins r1, immed=i}
      | ins as INS_SIR {opcode=opc, id=id, immed=i, r1=r1} =>
        INS_SIR {opcode=opc, id=id, immed=i, r1=f ins r1}
      | ins as INS_NEW {opcode=opc, dest=d, id=id, fields=fs} =>
        INS_NEW {opcode=opc, dest=f ins d, id=id, fields=fs}
      | ins as INS_RR  {opcode=opc, dest=d, r1=r1} =>
        INS_RR {opcode=opc, dest=f ins d, r1=f ins r1}
      | ins as INS_IR {opcode=opc, dest=d, immed=i} =>
        INS_IR {opcode=opc, dest=f ins d, immed=i}
      | ins as INS_RI {opcode=opc, dest=d, immed=i} =>
        INS_RI {opcode=opc, dest=f ins d, immed=i}
      | ins as INS_SR {opcode=opc, id=id, r1=r1} =>
        INS_SR {opcode=opc, id=id, r1=f ins r1}
      | ins as INS_RS {opcode=opc, id=id, r1=r1} =>
        INS_RS {opcode=opc, id=id, r1=f ins r1}
      | ins as INS_R {opcode=opc, r1=r1} =>
        INS_R {opcode=opc, r1=f ins r1}
      | ins => ins)


fun replaceSource copyIn reg =
    case pick (filter (fn (_, tgt) => tgt = reg) copyIn) of
        SOME ((src, _), _) => src
      | NONE => reg


fun replaceReg copyIn ins reg =
    let
        val (sources, _) = getST ins
    in
        if has reg sources then replaceSource copyIn reg else reg
    end


local
    fun bbCopyIn1 (LVA {gk=(gen, kill), copyIn=copyIn, ...}, preds) =
        intersection (preds, union (gen, difference (copyIn, kill)))


    fun transform1 (i, (ins, copyIn)) =
        let
            val (_, targets) = getST i
            val copyIn = filter (fn (src, tgt) => not (has src targets orelse
                                                       has tgt targets)) copyIn
        in
            (ins @ [replaceIns (replaceReg copyIn) i],
            case i of
                INS_RR {opcode=OP_MOV, r1=r1, dest=d} => add (copyIn, (r1, d))
              | i => copyIn)
        end


    fun transform copyIn ins =
        let
            val (newIns, _) = List.foldl transform1 ([], copyIn) ins
        in
            newIns
        end

in

    fun bbCopyIn (LVA {id=id, ins=ins, gk=gk, copyIn=copyIn, ...}, preds) =
        let
            val ci = List.foldr bbCopyIn1 (empty ()) preds
        in
            (LVA {
                  id=id,
                  ins=transform ci ins,
                  gk=gk,
                  copyIn=ci,
                  ciDiff=not (equal (copyIn, ci))
              },
             preds)
        end
end


local
    fun diffCheck ((LVA {ciDiff=ciD, ...}, _), diff) =
        if ciD then true else diff


    fun update1 newLvas (LVA {id=id, ...}) =
        #1 (valOf (List.find (fn (LVA {id=lId, ...}, _) => id = lId) newLvas))


    fun updateLvas lvas =
        let
            val newLvas = List.map bbCopyIn lvas
        in
            List.map (fn (l, ps) => (l, List.map (update1 newLvas) ps)) newLvas
        end
in
    fun mkCopyInSets lvas =
        if List.foldr diffCheck false lvas
        then mkCopyInSets (updateLvas lvas)
        else lvas
end


fun replaceCopiesBB lvas (id, _) =
    let
        (* Get the copyIn set corresponding with this bb.  *)
        val LVA {copyIn=copyIn, ins=ins, ...} =
            valOf (List.find (fn LVA {id=lId, ...} => id = lId) lvas)
    in
        (id, List.map (replaceIns (replaceReg copyIn)) ins)
    end


fun optFunc (id, cfg) =
    let
        val copies = List.foldr findCopies [] (Cfg.toList cfg)
        val lvas = (List.map #1
                    o mkCopyInSets
                    o mkPredRep (*TODO: Move to cfg structure, "toPredRep". *)
                    o Cfg.toListRep (*  Then we can replace two funcs w one. *)
                    o Cfg.map (bbToGK copies)) cfg
    in
        (id, Cfg.map (replaceCopiesBB lvas) cfg)
    end


fun copyProp prog = List.map optFunc prog

end
