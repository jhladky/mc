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


fun has thing L = List.exists (fn t => t = thing) L


(* DEBUGGING FUNCTIONS *)
fun copyToStr (src, dest) = "(" ^ rToStr src ^ ", " ^ rToStr dest ^ ")"


fun printGK id (gen, kill) =
    (print (id ^ "\ngen: [");
     print (Util.foldd ", " copyToStr (listItems gen));
     print "]\nkill: [";
     print (Util.foldd ", " copyToStr (listItems kill));
     print "]\n")
(* /DEBUGGING FUNCTIONS *)


(* If an instruction IS a copy and HAS NOT been killed add it to the gen set. *)
(* If the register target of an instruction HAS been involved in a copy
 * then add it to the kill set. *)
fun insToGK copies (gen, kill) (INS_RR {opcode=OP_MOV, r1=r1, dest=d}) =
    let
        val c = (r1, d)
    in
        (if not (member (kill, c)) then add (gen, c) else gen,
         add (kill, c))
    end
  | insToGK copies (gen, kill) ins =
    let
        (* The instruction IS NOT a copy so we add nothing to the gen set. *)
        val (_, targets) = getST ins
        val copies = filter (fn (src, tgt) => has src targets orelse
                                              has tgt targets) copies
    in
        (gen, union (kill, copies))
    end


(* fun bbToDFA copies (id, ins) = *)
(*     DFA { *)
(*         id=id, *)
(*         ins=ins, *)
(*         gk=List.foldr (fn (ins, gk) => insToGK copies gk ins) *)
(*                       (empty (), empty ()) ins, *)
(*         copyIn=empty (), *)
(*         diff=true *)
(*     } *)


fun bbToDFA copies (id, ins) =
    let
        val _ = print "/--------------\\\n";
        val gk = List.foldr (fn (i, gk) => insToGK copies gk i)
                            (empty (), empty ()) ins
    in
        printGK id gk;
        print "\\--------------/\n";
        DFA {id=id, ins=ins, gk=gk, copyIn=copies, diff=true}
    end


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
    has opc [OP_MOVEQ, OP_MOVNE, OP_MOVLT, OP_MOVGT, OP_MOVLE, OP_MOVGE]
  | isCondMove ins = false


fun replaceReg copyIn ins reg =
    let
        val (sources, _) = getST ins
    in
        if has reg sources andalso not (isCondMove ins)
        then case pick (filter (fn (_, tgt) => tgt = reg) copyIn) of
                 SOME ((src, _), _) => (
                  print ("Replaced [" ^ Util.iToS reg ^ "] with [" ^ Util.iToS src ^ "]\n");
                  (* print (insToStr ins ^ "\n"); *)
                  src
               )
               | NONE => reg
        else reg
    end


fun isTarget reg kill = exists (fn (_, tgt) => tgt = reg) kill


fun removeKilled copyIn kill =
    filter (fn (src, tgt) => not (isTarget src kill) andalso
                             not (isTarget tgt kill)) copyIn


fun bbCopyIn2 (DFA {gk=(gen, kill), copyIn=copyIn, ...}) =
    union (gen, removeKilled copyIn kill)


fun bbCopyIn1 (dfa, accum) = intersection (accum, bbCopyIn2 dfa)


fun bbCopyIn node =
    let
        val DFA {id=id, ins=ins, gk=gk, copyIn=copyIn, ...} = Cfg.getData node
        val preds = Cfg.getPreds node
        val ci = if length preds = 0 then empty ()
                 else List.foldr bbCopyIn1 (bbCopyIn2 (hd preds)) (tl preds)
        val diff = not (equal (copyIn, ci))
    in
        DFA {id=id, ins=ins, gk=gk, copyIn=ci, diff=diff}
    end


fun diffCheck (DFA {diff=d, ...}, diff) = if d then true else diff


fun printCopyIn node =
    let
        val da as DFA {id=id, copyIn=copyIn, ...} = Cfg.getData node
    in
        print (id ^ " copyIn: [" ^ Util.foldd ", " copyToStr (listItems copyIn) ^ "]\n");
        da
    end


fun buildLvas cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app bbCopyIn cfg; buildLvas cfg)
    else (Cfg.app printCopyIn cfg; cfg)


fun findCopies1 (INS_RR {opcode=OP_MOV, r1=r1, dest=d}, cps) = (r1, d)::cps
  | findCopies1 (ins, copies) = copies


fun findCopies ((_, ins), cps) = addList (cps, List.foldr findCopies1 [] ins)


fun replaceCopies1 (i, (ins, copyIn)) =
    let
        val (_, targets) = getST i
        val copyIn = filter (fn (src, tgt) => not (has src targets) andalso
                                              not (has tgt targets)) copyIn
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
        val lvas = buildLvas (Cfg.map (bbToDFA copies) cfg)
    in
        print "----------------------------\n";
        (id, Cfg.map replaceCopies lvas)
    end


fun copyProp prog = List.map optFunc prog

end
