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

(*we are going to need to change copies to a list and this remove this! *)
val nextCopy = ref 0

(* Just do this right now:
 * Pass through the entire function to determine what are the copies.
 * Give each copy a number. All set operations will work on those numbers.
 * This is what we're goign to be dealing with in this structure.
 * • Copy(i) is a set of pairs (u,v) such that v ← u is a copy
 * int * int * string * int source of copy, target of copy *)

fun findCopiesIns' copies (INS_RR {opcode=OP_MOV, r1=r1, dest=dest}) =
    (HashTable.insert copies (Int.toString (!nextCopy), (r1, dest));
     nextCopy := (!nextCopy) + 1)
  | findCopiesIns' _ _ = ()


fun findCopiesIns _ [] = ()
  | findCopiesIns copies (x::xs) =
    (findCopiesIns' copies x; findCopiesIns copies xs)


fun findCopiesBB copies _ (id, ins) =
    (findCopiesIns copies ins; (id, ins))


(*copyin depends on predecessors, or PUSH information down to successors. *)
(* If an instruction IS a copy and HAS NOT been killed add it to the gen set. *)
(* If the register target of an instruction HAS been involved in a copy (check the hash table),
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
        (* But we need to determine if it has been involved in a copy. *)
        val (_, targets) = getST ins
    in
        (gen, kill)
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


fun optFunc (id, cfg) =
    let
        val copies = Util.mkHt ()
        val _ = Cfg.apply (findCopiesBB copies) cfg;
        val lva = Cfg.map (bbToGK copies) cfg;
    in
        (id, cfg)
    end


fun copyProp prog = List.map optFunc prog


end
