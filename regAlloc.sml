signature REG_ALLOC = sig
    val regAlloc : TargetAmd64.program -> TargetAmd64.program
end

structure RegAlloc :> REG_ALLOC = struct
open TargetAmd64
open UnorderedSet

exception RegisterType of opcode

datatype live_analysis =
         LVA of {
             label: string,
             bb: instruction list,
             gk: set * set,
             liveOut: set,
             loDiff: bool
         }


(* Helper functions. *)
fun condAdd kill (reg, gen) =
    if not (member (kill, reg)) then add (gen, reg) else gen


fun condAddList (gen, kill) regs = List.foldr (condAdd kill) gen regs


fun getOffReg off = case off of SOME (r, _) => [r] | NONE => []


fun getSTrr r1 r2 OP_MOVQ  = ([r1], [r2])
  | getSTrr r1 r2 OP_ADDQ  = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_SUBQ  = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_IMULQ = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_ANDQ  = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_ORQ   = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_CMP   = ([r1, r2], [])
  | getSTrr _ _ opc        = raise RegisterType opc


fun getSTir r OP_SUBQ    = ([r], [r])
  | getSTir r OP_ADDQ    = ([r], [r])
  | getSTir r OP_XORQ    = ([r], [r])
  | getSTir r OP_CMP     = ([r], [])
  | getSTir r OP_MOVQ    = ([], [r])
  | getSTir r OP_CMOVEQ  = ([], [r])
  | getSTir r OP_CMOVNEQ = ([], [r])
  | getSTir r OP_CMOVLQ  = ([], [r])
  | getSTir r OP_CMOVGQ  = ([], [r])
  | getSTir r OP_CMOVLEQ = ([], [r])
  | getSTir r OP_CMOVGEQ = ([], [r])
  | getSTir _ opc = raise RegisterType opc


fun getSTkr r OP_SARQ = ([r], [r]) | getSTkr _ opc = raise RegisterType opc
fun getSTsr d OP_MOVQ = ([], [d]) | getSTsr _ opc = raise RegisterType opc
fun getSTgr d OP_MOVQ = ([], [d]) | getSTgr _ opc = raise RegisterType opc
fun getSTrg r OP_MOVQ = ([r], []) | getSTrg _ opc = raise RegisterType opc


fun getSTmr base offset dest OP_MOVQ = (base::getOffReg offset, [dest])
  | getSTmr _ _ _ opc = raise RegisterType opc


fun getSTrm reg base offset OP_MOVQ = (reg::base::getOffReg offset, [])
  | getSTrm _ _ _ opc = raise RegisterType opc


fun getSTr r OP_PUSHQ = ([r], [])
  | getSTr r OP_POPQ  = ([], [r])
  | getSTr r OP_IDIVQ = ([r], [])
  | getSTr _ opc = raise RegisterType opc


val getST =
 fn INS_RR {opcode=opc, r1=r1, r2=r2}                    => getSTrr r1 r2 opc
  | INS_IR {opcode=opc, r2=r2, ...}                      => getSTir r2 opc
  | INS_KR {opcode=opc, r2=r2, ...}                      => getSTkr r2 opc
  | INS_SR {opcode=opc, dest=d, ...}                     => getSTsr d opc
  | INS_GR {opcode=opc, dest=d, ...}                     => getSTgr d opc
  | INS_RG {opcode=opc, r1=r1, ...}                      => getSTrg r1 opc
  | INS_MR {opcode=opc, base=b, dest=d, offset=off, ...} => getSTmr b off d opc
  | INS_RM {opcode=opc, r1=r, base=b, offset=off, ...}   => getSTrm r b off opc
  | INS_R  {opcode=opc, r1=r1}                           => getSTr r1 opc
  | _                                                    => ([], [])


fun getNode ife reg =
    case IfeGraph.find ife reg of
        SOME node => node
      | NONE => IfeGraph.mkNode ife reg


(* Transformation functions. *)
fun regToGK (gen, kill) ins =
    let
        val (sources, targets) = getST ins
    in
        (condAddList (gen, kill) sources, addList (kill, targets))
    end


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
    if diffCheck cfg then (Cfg.apply bbLiveOut cfg; (*print ("LENGTH " ^ Int.toString (numItems let val LVA {liveOut=liveOut, ...} = Cfg.getData (Cfg.getExit cfg) in liveOut end) ^ "\n"); *)funcLiveOut (id, cfg))
    else (id, cfg)


fun addEdge ife r1 r2 =
    let
        val node1 = getNode ife r1
        val node2 = getNode ife r2
    in
        IfeGraph.addEdge node1 node2
    end


fun insIfeGraph ife (ins, liveNow) =
    let
        val (sources, targets) = getST ins
    in
        List.app (fn t => app (addEdge ife t) liveNow) targets;
        (*since we're going to modify the livenow set, we have to pass it back*)

        (*this exception is a problem.... what's going on here...*)
        addList (List.foldr (fn (t, ln) => delete (ln, t) handle NotFound => ln) liveNow targets,
                 sources)
    end


(* This function will be passed the successors of the node as part of
 * how Cfg.apply is written, but we don't need them here. *)
fun bbIfeGraph ife _ (lva as LVA {bb=bb, liveOut=liveOut, ...}) =
    (*recall bb is the instruction list*)
    (* what do we do with the 'liveNow' set / lva
     * once we're done with it on each bb*)
    let
        (*liveOut comes out of here*)
        val liveOut_final = List.foldr (insIfeGraph ife) liveOut bb
    in
        print ("length: " ^ Int.toString (numItems liveOut_final) ^ "\n");
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
