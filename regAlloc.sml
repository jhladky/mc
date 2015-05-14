signature REG_ALLOC = sig
    val regAlloc : TargetAmd64.program -> TargetAmd64.program
end

structure RegAlloc :> REG_ALLOC = struct
open TargetAmd64
open UnorderedSet

(* Function should not be called on a register of this type. *)
exception RegisterType of opcode

datatype live_analysis =
         LVA of {
             label: string,
             bb: instruction list,
             gk: register set * register set,
             liveOut: register set,
             loDiff: bool
         }

(* Set of all the actual i.e. not virtual, registers. *)
val actual = addList (empty (), [REG_RAX, REG_RBX, REG_RCX, REG_RDX,
                                 REG_RSI, REG_RDI, REG_RBP, REG_RSP])

(* Set of all the registers we have available for coloring. *)
val avail = addList (empty (), [REG_8, REG_9, REG_10, REG_11,
                                REG_12, REG_13, REG_14, REG_15,
                                REG_RAX, REG_RBX, REG_RCX, REG_RDX,
                                REG_RSI, REG_RDI, REG_RBP])

(* Registers that must be preserved in the function i.e. callee saved. *)
val preserved = addList (empty (), [REG_RBX, REG_RSP, REG_RBP,
                                    REG_12, REG_13, REG_14, REG_15])

(* Registers that can be used as scratch i.e. caller saved. *)
val scratch = addList (empty (), [REG_RAX, REG_RDI, REG_RSI, REG_RDX, REG_RCX,
                                  REG_8, REG_9, REG_10, REG_11])

(* Helper functions. *)
fun condAdd kill (reg, gen) =
    if not (member (kill, reg)) then add (gen, reg) else gen


fun condAddList (gen, kill) regs = List.foldr (condAdd kill) gen regs


fun getOffReg off = case off of SOME (r, _) => [r] | NONE => []


fun getSTrr r1 r2 OP_MOVQ   = ([r1], [r2])
  | getSTrr r1 r2 OP_ADDQ   = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_SUBQ   = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_IMULQ  = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_ANDQ   = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_ORQ    = ([r1, r2], [r2])
  | getSTrr r1 r2 OP_CMP    = ([r1, r2], [])
  | getSTrr r1 r2 OP_CMOVE  = ([r1], [r1, r2])
  | getSTrr r1 r2 OP_CMOVNE = ([r1], [r1, r2])
  | getSTrr r1 r2 OP_CMOVL  = ([r1], [r1, r2])
  | getSTrr r1 r2 OP_CMOVG  = ([r1], [r1, r2])
  | getSTrr r1 r2 OP_CMOVLE = ([r1], [r1, r2])
  | getSTrr r1 r2 OP_CMOVGE = ([r1], [r1, r2])
  | getSTrr _ _ opc         = raise RegisterType opc


fun getSTir r OP_SUBQ    = ([r], [r])
  | getSTir r OP_ADDQ    = ([r], [r])
  | getSTir r OP_XORQ    = ([r], [r])
  | getSTir r OP_CMP     = ([r], [])
  | getSTir r OP_MOVQ    = ([], [r])
  | getSTir r OP_SARQ    = ([r], [r])
  | getSTir _ opc        = raise RegisterType opc


fun getSTsr d OP_MOVQ = ([], [d]) | getSTsr _ opc = raise RegisterType opc
fun getSTgr d OP_MOVQ = ([], [d]) | getSTgr _ opc = raise RegisterType opc
fun getSTrg r OP_MOVQ = ([r], []) | getSTrg _ opc = raise RegisterType opc


fun getSTmr base offset dest OP_MOVQ = (base::getOffReg offset, [dest])
  | getSTmr _ _ _ opc = raise RegisterType opc


fun getSTrm reg base offset OP_MOVQ = (reg::base::getOffReg offset, [])
  | getSTrm _ _ _ opc = raise RegisterType opc


fun getSTr r OP_PUSHQ = ([r], [])
  | getSTr r OP_POPQ  = ([], [r])
  | getSTr r OP_IDIVQ = ([REG_RAX, REG_RDX, r], [REG_RAX])
  | getSTr _ opc      = raise RegisterType opc


val getST =
 fn INS_RR {opcode=opc, r1=r1, r2=r2}                    => getSTrr r1 r2 opc
  | INS_IR {opcode=opc, r2=r2, ...}                      => getSTir r2 opc
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


fun addEdge ife r1 r2 =
    let
        val node1 = getNode ife r1
        val node2 = getNode ife r2
    in
        IfeGraph.addEdge node1 node2
    end


fun addEdgeNoCreate ife node reg =
    case IfeGraph.find ife reg of
        SOME other => IfeGraph.addEdge node other
      | NONE => ()


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


fun insIfeGraph ife (ins, liveNow) =
    let
        val (sources, targets) = getST ins
    in
        List.app (fn t => app (addEdge ife t) liveNow) targets;
        (*since we're going to modify the livenow set, we have to pass it back*)
        addList (List.foldr (fn (t, ln) => delete (ln, t) handle NotFound => ln)
                            liveNow targets, sources)
    end


(* This function will be passed the successors of the node as part of
 * how Cfg.apply is written, but we don't need them here. *)
fun bbIfeGraph ife _ (lva as LVA {bb=bb, liveOut=liveOut, ...}) =
    (*liveOut comes out of here*)
    (List.foldr (insIfeGraph ife) liveOut bb; lva)


fun uncReq (reg, adjs) = length adjs < numItems avail andalso
                         not (member (actual, reg))


fun conReq (reg, adjs) = length adjs >= numItems avail andalso
                         not (member (actual, reg))


(*TODO: Make this much better, by actually using the heuristic and by
 * reanalyzing the graph every time.*)
fun deconstruct ife =
    (*Right now we need to get the adjacency information for each node.*)
    let
        val rep = IfeGraph.toListRep ife
    in
        (* Unconstrained nodes will form the base of our stack. *)
        List.filter (fn (reg, _) => member (actual, reg)) rep
        @ List.filter conReq rep
        @ List.filter uncReq rep
    end


fun addRealRegToSet vtr (REG_V n, used) =
    (case HashTable.find vtr (Int.toString n) of
         SOME reg => add (used, reg)
       | NONE => used)
  | addRealRegToSet vtr (reg, used) = add (used, reg)


(* vtr  : Virtual-to-real register mapping,
 * adjs : List of virtual neighbor registers.
 *        Returns available registers.*)
fun getAvailRegs vtr adjs =
    difference (avail, List.foldr (addRealRegToSet vtr) (empty ()) adjs)


(* Return a reg option, it is SOME reg then we have to spill. *)
fun addToIfe vtr ife (REG_V n, adjs) =
    (* Here we're dealing with a virtual register. *)
    (case pick (getAvailRegs vtr adjs) of
         (* `Pick` also returns the rest of the list,
          * we're not going to use it right now.*)
         SOME (reg, _) =>
         (List.app (addEdgeNoCreate ife (IfeGraph.mkNode ife reg)) adjs;
          HashTable.insert vtr (Int.toString n, reg);
          NONE)
       | NONE => SOME (REG_V n))
  | addToIfe vtr ife (reg, adjs) =
    (List.app (addEdgeNoCreate ife (IfeGraph.mkNode ife reg)) adjs; NONE)


fun replace vtr (REG_V n) = HashTable.lookup vtr (Int.toString n)
  | replace vtr reg = reg


fun replaceOffset vtr (SOME (reg, scalar)) = SOME (replace vtr reg, scalar)
  | replaceOffset vtr NONE = NONE


fun getSaveList vtr =
  listItems (intersection (addList (empty(), HashTable.listItems vtr),
                           preserved))


(* TODO: for the call instructions all the caller saved registers are considered
 * targets, which means that any virtual registers that span the call will
 * have an edge with the caller saved registers, forcing them into the callee
 * saved registers. *)
fun colorCall vtr label =
    let
        val save = intersection (addList (empty(), HashTable.listItems vtr),
                                 scratch)
        val save = listItems save
    in
        List.map (fn reg => INS_R {opcode=OP_PUSHQ, r1=reg}) save
        @ [INS_L {opcode=OP_CALL, label=label}]
        @ List.rev (List.map (fn reg => INS_R {opcode=OP_POPQ, r1=reg}) save)
    end


fun colorIns vtr (INS_RR {opcode=opc, r1=r1, r2=r2}) =
    [INS_RR {opcode=opc, r1=replace vtr r1, r2=replace vtr r2}]
  | colorIns vtr (INS_IR {opcode=opc, immed=immed, r2=r2}) =
    [INS_IR {opcode=opc, immed=immed, r2=replace vtr r2}]
  | colorIns vtr (INS_GR {opcode=opc, global=glob, dest=dest}) =
    [INS_GR {opcode=opc, global=glob, dest=replace vtr dest}]
  | colorIns vtr (INS_RG {opcode=opc, r1=r1, global=glob}) =
    [INS_RG {opcode=opc, global=glob, r1=replace vtr r1}]
  | colorIns vtr (INS_SR {opcode=opc, id=id, dest=dest}) =
    [INS_SR {opcode=opc, id=id, dest=replace vtr dest}]
  | colorIns vtr (INS_R {opcode=opc, r1=r1}) =
    [INS_R {opcode=opc, r1=replace vtr r1}]
  | colorIns vtr (INS_MR {opcode=opc, immed=i, base=b, offset=off, dest=d}) =
    [INS_MR {opcode=opc, immed=i, base=replace vtr b,
             offset=replaceOffset vtr off, dest=replace vtr d}]
  | colorIns vtr (INS_RM {opcode=opc, r1=r1, immed=i, base=b, offset=off}) =
    [INS_RM {opcode=opc, immed=i, r1=replace vtr r1, base=replace vtr b,
             offset=replaceOffset vtr off}]
  | colorIns vtr (INS_L {opcode=OP_CALL, label=label}) =
    colorCall vtr label
  | colorIns vtr (INS_X {opcode=OP_RET}) =
    List.rev (List.map (fn reg => INS_R {opcode=OP_POPQ, r1=reg})
                       (getSaveList vtr)) @ [INS_X {opcode=OP_RET}]
  | colorIns _ ins = [ins]


(* Iteratively recompute each liveOut set until there is no change. *)
fun mkLiveOutSets cfg =
    if diffCheck cfg then (Cfg.apply bbLiveOut cfg; mkLiveOutSets cfg)
    else cfg


(* Calculate how much spill space we need. Make sure it is 16-byte aligned. *)
fun calcStackOffset existing n =
  let
      val new = existing div Util.WORD_SIZE + n
  in
      (if new mod 2 = 0 then new + 1 else new) * Util.WORD_SIZE
  end


(* Update the top-of-stack modifying instructions to make sure we
 * reserved enough spill space. *)
fun updatePP n (INS_IR {opcode=OP_SUBQ, r2=REG_RSP, immed=immed}) =
    INS_IR {opcode=OP_SUBQ, r2=REG_RSP, immed=calcStackOffset immed n}
  | updatePP n (INS_IR {opcode=OP_ADDQ, r2=REG_RSP, immed=immed}) =
    INS_IR {opcode=OP_ADDQ, r2=REG_RSP, immed=calcStackOffset immed n}
  | updatePP _ ins = ins


(* n      : Number of times we've spilled.
 * funcId : Name of the function. We use it to find the entry BB.
 * vtr    : Virtual-to-real register mapping*)
fun colorBB n funcId vtr (id, ins) =
    let
        val L = if id = funcId
                then List.map (fn reg => INS_R {opcode=OP_PUSHQ, r1=reg})
                              (getSaveList vtr)
                else []
        val L = L @ List.foldr (fn (ins, L) => colorIns vtr ins @ L) [] ins
    in
        (id, List.map (updatePP n) L)
    end


fun buildVtr f [] = NONE
  | buildVtr f (node::nodes) =
    case f node of SOME reg => SOME reg | NONE => buildVtr f nodes


fun spillReg n reg ins =
    let
        val (sources, targets) = getST ins
    in
        if List.exists (fn r => r = reg) sources then
            [INS_MR {opcode=OP_MOVQ, immed=(~((n + 1) * Util.WORD_SIZE)),
                     base=REG_RBP, dest=reg, offset=NONE},
             ins]
        else if List.exists (fn r => r = reg) targets then
            [ins,
             INS_RM {opcode=OP_MOVQ, immed=(~((n + 1) * Util.WORD_SIZE)),
                     base=REG_RBP, offset=NONE, r1=reg}]
        else
            [ins]
  end


fun spill n reg (id, ins) =
    (id, List.foldr (fn (ins, L) => spillReg n reg ins @ L) [] ins)


(* n is the number of times we've had to spill. *)
fun color n (func as (id, cfg)) =
    let
        val lva = mkLiveOutSets (Cfg.map bbToGK cfg)
        val oldIfe = IfeGraph.mkGraph ()
        val newIfe = IfeGraph.mkGraph ()
        val vtr = Util.mkHt ()
    in
        (* Build the interference graph. *)
        Cfg.apply (bbIfeGraph oldIfe) lva;
        (* Build the vtr. If we can color the registers then map them
         * to the old ones, otherwise generate spill code and try again. *)
        case buildVtr (addToIfe vtr newIfe) (deconstruct oldIfe) of
            SOME reg => color (n + 1) (id, Cfg.map (spill n reg) cfg)
          | NONE => (id, Cfg.map (colorBB n id vtr) cfg)
    end


fun regAlloc (PROGRAM {text=text, data=data}) =
    PROGRAM {text=List.map (fn func => color 0 func) text, data=data}

end
