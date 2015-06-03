signature COPY_PROP = sig
    val copyProp : Iloc.program -> Iloc.program
end

structure CopyProp :> COPY_PROP = struct
open Iloc
open UnorderedSet

type copy = int * int

datatype dataflow_analysis =
         DFA of {
             id: string,
             ins: instruction list,
             gk: copy set * copy set,
             copyIn: copy set,
             diff: bool
         }


fun involved t (src, tgt) = src = t orelse tgt = t


(* If an instruction is a copy and HAS NOT been killed add it to the gen set.
 * If the target of an instruction HAS been involved in a copy then add it to
 * the kill set. *)
fun iToGK copies (ins, (gen, kill)) =
    let
        val copies = case #2 (getST ins) of
                         NONE => copies
                       | SOME t => filter (involved t) copies
    in
        case ins of
            INS_RR {opcode=OP_MOV, r1=r1, dest=d} =>
            (if not (exists (fn (src, tgt) => tgt = d orelse src = d) kill)
             then add (gen, (r1, d))
             else gen,
             add (union (kill, copies), (r1, d)))
          | _ => (gen, union (kill, copies))
    end


fun insToGK copies (id, ins) =
    List.foldr (iToGK copies) (empty (), empty ()) ins


fun bbToDFA copies (id, ins) =
    DFA {id=id, ins=ins, gk=insToGK copies (id, ins), copyIn=copies, diff=true}


fun replaceIns f ins =
    case ins of
        INS_RRR {opcode=c, dest=d, r1=r1, r2=r2} =>
        INS_RRR {opcode=c, dest=f ins d, r1=f ins r1, r2=f ins r2}
      | INS_RIR {opcode=c, dest=d, r1=r1, immed=i} =>
        INS_RIR {opcode=c, dest=f ins d, r1=f ins r1, immed=i}
      | INS_RRI {opcode=c, r1=r1, r2=r2, immed=i} =>
        INS_RRI {opcode=c, r1=f ins r1, r2=f ins r2, immed=i}
      | INS_RRC {opcode=c, r1=r1, r2=r2} =>
        INS_RRC {opcode=c, r1=f ins r1, r2=f ins r2}
      | INS_RIC {opcode=c, r1=r1, immed=i} =>
        INS_RIC {opcode=c, r1=f ins r1, immed=i}
      | INS_SIR {opcode=c, id=id, immed=i, r1=r1} =>
        INS_SIR {opcode=c, id=id, immed=i, r1=f ins r1}
      | INS_NEW {opcode=c, dest=d, id=id, fields=fs} =>
        INS_NEW {opcode=c, dest=f ins d, id=id, fields=fs}
      | INS_RR  {opcode=c, dest=d, r1=r1} =>
        INS_RR  {opcode=c, dest=f ins d, r1=f ins r1}
      | INS_IR  {opcode=c, dest=d, immed=i} =>
        INS_IR  {opcode=c, dest=f ins d, immed=i}
      | INS_RI  {opcode=c, dest=d, immed=i} =>
        INS_RI  {opcode=c, dest=f ins d, immed=i}
      | INS_SR  {opcode=c, id=id, r1=r1} =>
        INS_SR  {opcode=c, id=id, r1=f ins r1}
      | INS_RS  {opcode=c, id=id, r1=r1} =>
        INS_RS  {opcode=c, id=id, r1=f ins r1}
      | INS_R   {opcode=c, r1=r1} => INS_R {opcode=c, r1=f ins r1}
      | _ => ins


fun isCondMove (INS_RR {opcode=opc, ...}) =
    Util.has opc [OP_MOVEQ, OP_MOVNE, OP_MOVLT, OP_MOVGT, OP_MOVLE, OP_MOVGE]
  | isCondMove _ = false


(* fun replaceReg copyIn ins reg = *)
(*     let *)
(*         val (sources, _) = getST ins *)
(*     in *)
(*         if Util.has reg sources andalso not (isCondMove ins) *)
(*         then case pick (filter (fn (_, tgt) => tgt = reg) copyIn) of *)
(*                  SOME ((src, _), _) => src *)
(*                | NONE => reg *)
(*         else reg *)
(*     end *)

fun replaceReg copyIn ins reg =
    if Util.has reg (#1 (getST ins)) andalso not (isCondMove ins)
    then case pick (filter (fn (_, tgt) => tgt = reg) copyIn) of
             SOME ((src, _), _) =>
             (print ("Replaced [" ^ Util.iToS reg ^ "] with [" ^ Util.iToS src ^ "] in " ^  insToStr ins ^ "\n");
              src)
           | NONE => reg
    else reg


fun isTarget reg kill = exists (fn (_, tgt) => tgt = reg) kill


fun removeKilled copyIn kill =
    filter (fn (src, tgt) => not (isTarget src kill) andalso
                             not (isTarget tgt kill)) copyIn


fun propagate2 (DFA {gk=(gen, kill), copyIn=copyIn, ...}) =
    union (gen, removeKilled copyIn kill)


fun propagate1 (dfa, accum) = intersection (accum, propagate2 dfa)


fun propagate node =
    let
        val DFA {id=id, ins=ins, gk=gk, copyIn=copyIn, ...} = Cfg.getData node
        val preds = Cfg.getPreds node
        val ci = if length preds = 0 then empty ()
                 else List.foldr propagate1 (propagate2 (hd preds)) (tl preds)
        val diff = not (equal (copyIn, ci))
    in
        DFA {id=id, ins=ins, gk=gk, copyIn=ci, diff=diff}
    end


fun diffCheck (DFA {diff=d, ...}, diff) = if d then true else diff


fun buildDFAs cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app propagate cfg; buildDFAs cfg)
    else cfg


fun findCopies1 (INS_RR {opcode=OP_MOV, r1=r1, dest=d}, cps) = (r1, d)::cps
  | findCopies1 (ins, copies) = copies


fun findCopies ((_, ins), cps) = addList (cps, List.foldr findCopies1 [] ins)


fun replaceCopies1 (i, (ins, copyIn)) =
    let
        val copyIn = case #2 (getST i) of
                         NONE => copyIn
                       | SOME t => filter (fn st => not (involved t st)) copyIn
    in
        (ins @ [replaceIns (replaceReg copyIn) i],
         case i of
             INS_RR {opcode=OP_MOV, r1=r1, dest=d} => add (copyIn, (r1, d))
           | i => copyIn)
    end


fun replaceCopies (DFA {id=id, ins=ins, copyIn=copyIn, ...}) =
    (id, #1 (List.foldl replaceCopies1 ([], copyIn) ins))


fun optFunc (id, cfg) =
    let
        val copies = Cfg.fold findCopies (empty ()) cfg
        val lvas = buildDFAs (Cfg.map (bbToDFA copies) cfg)
        fun dash s n = if n = 0 then s else dash (s ^ "-") (n - 1)
    in
        print ("/-----" ^ id ^ "-----\\\n");
        (id, Cfg.map replaceCopies lvas) before
        print ("\\" ^ dash "" ((size id) + 10) ^ "/\n")
    end


fun copyProp prog = List.map optFunc prog

end
