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


fun condAdd kill (reg, gen) =
    if not (member (kill, reg)) then add (gen, reg) else gen


fun condAddList (gen, kill) regs = List.foldr (condAdd kill) gen regs


fun getOffReg off = case off of SOME (r, _) => [r] | NONE => []


val getSources =
 fn INS_RR {r1=r1, ...}                     => [r1]
  | INS_RG {r1=r1, ...}                     => [r1]
  | INS_MR {base=b, offset=off, ...}        => b::getOffReg off
  | INS_RM {r1=r1, base=b, offset=off, ...} => r1::b::getOffReg off
  | INS_R {r1=r1, ...}                      => [] (*think this is wrong...*)
  | _                                       => []


val getTargets =
 fn INS_RR {r2=r2, ...}        => [r2]
  | INS_IR {r2=r2, ...}        => [r2]
  | INS_KR {r2=r2, ...}        => [r2]
  | INS_SR {dest=dest, ...}    => [dest]
  | INS_GR {dest=dest, ...}    => [dest]
  | INS_MR {dest=dest, ...}    => [dest]
  | INS_R {r1=r1, ...}         => [r1] (*think this is wrong....*)
  | _                          => []


fun regToGK (gen, kill) ins =
    (condAddList (gen, kill) (getSources ins), addList (kill, getTargets ins))


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


(* for each instruction in Block, from the bottom to the top
 *     add the edge from target to each register in LiveNow set
 *     remove target from LiveNow set
 *     add all sources to the LiveNow set *)


(* This function will be passed the successors of the node as part of
 * how Cfg.apply is written, but we don't need them here. *)
fun bbIfeGraph ife _ (lva as LVA {bb=bb, liveOut=liveOut, ...}) =
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
