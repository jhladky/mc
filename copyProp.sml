signature COPY_PROP = sig
    val copyProp : Iloc.program -> Iloc.program
end

structure CopyProp :> COPY_PROP = struct
open Iloc
open UnorderedSet

type copy = int * int

datatype live_variable_analysis =
         LVA of {
             ins: instruction list,
             gk: copy set * copy set,
             copyIn: copy set,
             ciDiff: bool
         }


(* Just do this right now:
 * Pass through the entire function to determine what are the copies.
 * Give each copy a number. All set operations will work on those numbers.
 * This is what we're goign to be dealing with in this structure.
 * • Copy(i) is a set of pairs (u,v) such that v ← u is a copy
 * int * int * string * int source of copy, target of copy *)

fun findCopiesIns (INS_RR {opcode=OP_MOV, r1=r1, dest=dest}, copies) =
    (r1, dest)::copies
  | findCopiesIns (ins, copies) = copies


fun findCopiesBB ((id, ins), cps) = cps @ List.foldr findCopiesIns [] ins


(* Does the weird conditional move thing still apply??? *)


fun has reg L = List.exists (fn r => r = reg) L


(*copyin depends on predecessors, or PUSH information down to successors. *)
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
        (* But we need to determine if it has been ___involved___ (so in any way???) in a copy. *)
        val (_, targets) = getST ins
        (* so that kills the copy! OK *)
        val copies = List.filter (fn (src, tgt) => has src targets orelse has tgt targets) copies
    in
        (gen, addList (kill, copies))
    end


fun bbToGK copies (id, ins) =
    LVA {
        ins=ins,
        gk=List.foldr (fn (ins, gk) => insToGK copies gk ins)
                      (empty (), empty ()) ins,
        copyIn=empty (),
        ciDiff=true
    }


fun diffCheck ((LVA {ciDiff=ciD, ...}, _), diff) = if ciD then true else diff


(* CopyIn(n) = the intersection of (all sets m for m in pred(n)) gen(m) U (CopyIn(m) - kill(m)) *)

fun bbCopyIn (LVA {ins=ins, gk=gk, copyIn=copyIn, ...}, succs) =
    let
    in
        (* Fix me! *)
        (LVA {ins=ins, gk=gk, copyIn=copyIn, ciDiff=false}, succs)
    end


fun mkCopyInSets lvas =
    if List.foldr diffCheck false lvas
    then mkCopyInSets (List.map bbCopyIn lvas)
    else lvas


fun optFunc (id, cfg) =
    let
        val copies = List.foldr findCopiesBB [] (Cfg.toList cfg)
        (* val lvas = Cfg.map (bbToGK copies) cfg *)
        val lvas = mkCopyInSets (Cfg.toListRep (Cfg.map (bbToGK copies) cfg))
    in
        (id, cfg)
    end


fun copyProp prog = List.map optFunc prog


end
