signature REG_ALLOC = sig
    val regAlloc : TargetAmd64.program -> TargetAmd64.program
end

structure RegAlloc :> REG_ALLOC = struct
open TargetAmd64
open UnorderedSet

datatype live_analysis =
         LVA of {
             label: string,
             bb: instruction list,
             gk: set * set,
             liveOut: set,
             loDiff: bool
         }


fun condAdd (gen, kill) reg =
    if not (member (kill, reg)) then add (gen, reg) else gen


fun regToGK (gen, kill) (INS_RR {r1=r1, r2=r2, ...}) =
    (condAdd (gen, kill) r1, add (kill, r2))
  | regToGK (gen, kill) (INS_IR {immed=immed, r2=r2, ...}) =
    (gen, add (kill, r2))
  | regToGK (gen, kill) (INS_KR {k=k, r2=r2, ...}) =
    (gen, add (kill, r2))
  | regToGK (gen, kill) (INS_SR {id=id, dest=dest, ...}) =
    (gen, add (kill, dest))
  | regToGK (gen, kill) (INS_GR {global=global, dest=dest, ...}) =
    (gen, add (kill, dest))
  | regToGK (gen, kill) (INS_RG {r1=r1, global=global, ...}) =
    (condAdd (gen, kill) r1, kill)
  | regToGK (gen, kill) (INS_MR {immed=immed, base=base, dest=dest,
                                 offset=offset, ...}) =
    let
        val gen = condAdd (gen, kill) base
    in
        (case offset of SOME (r, _) => condAdd (gen, kill) r | NONE => gen,
         add (kill, dest))
    end
  | regToGK (gen, kill) (INS_RM {r1=r1, immed=immed, base=base,
                                 offset=offset, ...}) =
    let
        val kill = add (kill, base)
    in
        (condAdd (gen, kill) r1,
         case offset of SOME (r, _) => add (kill, r) | NONE => kill)
    end
  | regToGK (gen, kill) (INS_R {r1=r1, ...}) = (*pretty sure this is wrong*)
    (gen, add (kill, r1))
  (* The remaining instruction types don't involve any registers. *)
  | regToGK gk _ = gk


fun bbToGK (id, bb) =
    let
        val gk = List.foldl (fn (ins, gk) => regToGK gk ins)
                            (empty (), empty ()) bb
    in
        LVA {label=id, bb=bb, gk=gk, liveOut=empty (), loDiff=true}
    end


(* Function level.*)
fun funcToGK (id, cfg) = (id, Cfg.map bbToGK cfg)


fun bbLiveOut1 (LVA {gk=(gen, kill), liveOut=liveOut, ...}) =
    union (gen, difference (liveOut, kill))


fun bbLiveOut succs (LVA {label=id, bb=bb, gk=gk, liveOut=liveOut, ...}) =
    let
        val lo = List.foldr (fn (bb, s) => union (s, bbLiveOut1 bb))
                            (empty ()) succs
    in
        LVA {label=id, bb=bb, gk=gk, liveOut=lo, loDiff=not (equal (liveOut, lo))}
    end


fun diffCheck1 diff [] = diff
  | diffCheck1 diff (LVA {loDiff=loDiff, ...}::lvas) =
    if loDiff then diffCheck1 true lvas else diffCheck1 diff lvas


fun diffCheck cfg = diffCheck1 false (Cfg.toList cfg)


(* This is where we have to keep doing the liveOut thing
 * Iteratively recompute liveout until there is no change *)
fun funcLiveOut (id, cfg) =
    if diffCheck cfg then (Cfg.apply bbLiveOut cfg; funcLiveOut (id, cfg))
    else (id, cfg)


(* for each instruction in Block, from the bottom to the top *)
(*     add the edge from target to each register in LiveNow set *)
(*     remove target from LiveNow set *)
(*     add all sources to the LiveNow set *)


(* This function will be passed the successors of the node as part of
 * how Cfg.apply is written, but we don't need them here. *)
fun bbIfeGraph ife _ (lva as LVA {...}) =
    (*what do we do with the lva now that we have it????*)
    let
    in
        lva
    end


(*build the interferance graph here*)
fun funcIfeGraph (id, cfg) =
    let
        val ife = IfeGraph.mkGraph ()
    in
        Cfg.apply (bbIfeGraph ife) cfg;
        (id, ife)
    end


fun regAlloc (p as PROGRAM {text=text, data=data}) =
    let
        val funcs = List.map funcLiveOut (List.map funcToGK text)
        val _ = List.map funcIfeGraph funcs
    in
        p (*just sending the same program back at this point*)
    end

end
