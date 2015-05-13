signature REG_ALLOC = sig
    val regAlloc : TargetAmd64.program -> TargetAmd64.program
end

structure RegAlloc :> REG_ALLOC = struct
open TargetAmd64
open UnorderedSet

(* Function should not be called on a register of this type. *)
exception RegisterType of opcode

(* There is no real register in the Virtual-To-Real register table.
 * Indicates that register coloring failed. *)
exception NoRealRegister of register

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
val avail = addList (empty (), [REG_N 8, REG_N 9, REG_N 10, REG_N 11,
                                REG_N 12, REG_N 13, REG_N 14, REG_N 15,
                                REG_RAX, REG_RBX, REG_RCX, REG_RDX,
                                REG_RSI, REG_RDI, REG_RBP])

(* Registers that must be preserved in the function i.e. callee saved. *)
val preserved = addList (empty (), [REG_RBX, REG_RSP, REG_RBP,
                                    REG_N 12, REG_N 13, REG_N 14, REG_N 15])

(* Registers that can be used as scratch i.e. caller saved. *)
val scratch = addList (empty (), [REG_RAX, REG_RDI, REG_RSI, REG_RDX, REG_RCX,
                                  REG_N 8, REG_N 9, REG_N 10, REG_N 11])

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


(* fun getSTkr r OP_SARQ = ([r], [r]) | getSTkr _ opc = raise RegisterType opc *)
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
  | getSTr _ opc = raise RegisterType opc


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


fun addRealRegToSet vtr (REG_N n, used) =
    (case HashTable.find vtr (Int.toString n) of
         SOME reg => add (used, reg)
       | NONE => used)
  | addRealRegToSet vtr (reg, used) = add (used, reg)


(* vtr : virtual to real mapping,
 adjs : list of virtual neighbor registers
 returns available registers*)
fun getAvailRegs vtr adjs =
    difference (avail, List.foldr (addRealRegToSet vtr) (empty ()) adjs)


fun spill () = (print "SPILL!\n"; OS.Process.exit OS.Process.failure)


fun addEdgeNoCreate ife node reg =
    case IfeGraph.find ife reg of
        SOME other => IfeGraph.addEdge node other
      | NONE => ()


fun addToIfe vtr ife (REG_N n, adjs) =
    (*here we're dealing with a virtual register*)
    (case pick (getAvailRegs vtr adjs) of
         SOME (reg, _) => (* `Pick` also returns the rest of the list,
                           * we're not going to use it right now.*)
         (List.app (addEdgeNoCreate ife (IfeGraph.mkNode ife reg)) adjs;
          HashTable.insert vtr (Int.toString n, reg))
       | NONE => spill ()) (*do the SPILL stuff here*)
  | addToIfe vtr ife (reg, adjs) =
    List.app (addEdgeNoCreate ife (IfeGraph.mkNode ife reg)) adjs


fun replace vtr (REG_N n) = HashTable.lookup vtr (Int.toString n)
  | replace vtr reg = reg


fun replaceOffset vtr (SOME (reg, scalar)) = SOME (replace vtr reg, scalar)
  | replaceOffset vtr NONE = NONE


fun colorIns vtr (INS_RR {opcode=opc, r1=r1, r2=r2}) =
    INS_RR {opcode=opc, r1=replace vtr r1, r2=replace vtr r2}
  | colorIns vtr (INS_IR {opcode=opc, immed=immed, r2=r2}) =
    INS_IR {opcode=opc, immed=immed, r2=replace vtr r2}
  | colorIns vtr (INS_GR {opcode=opc, global=glob, dest=dest}) =
    INS_GR {opcode=opc, global=glob, dest=replace vtr dest}
  | colorIns vtr (INS_RG {opcode=opc, r1=r1, global=glob}) =
    INS_RG {opcode=opc, global=glob, r1=replace vtr r1}
  | colorIns vtr (INS_SR {opcode=opc, id=id, dest=dest}) =
    INS_SR {opcode=opc, id=id, dest=replace vtr dest}
  | colorIns vtr (INS_R {opcode=opc, r1=r1}) =
    INS_R {opcode=opc, r1=replace vtr r1}
  | colorIns vtr (INS_MR {opcode=opc, immed=i, base=b, offset=off, dest=d}) =
    INS_MR {opcode=opc, immed=i, base=replace vtr b,
            offset=replaceOffset vtr off, dest=replace vtr d}
  | colorIns vtr (INS_RM {opcode=opc, r1=r1, immed=i, base=b, offset=off}) =
    INS_RM {opcode=opc, immed=i, r1=replace vtr r1, base=replace vtr b,
            offset=replaceOffset vtr off}
  | colorIns _ ins = ins


(* Iteratively recompute each liveOut set until there is no change. *)
fun mkLiveOutSets cfg =
    if diffCheck cfg then (Cfg.apply bbLiveOut cfg; mkLiveOutSets cfg)
    else cfg


fun color (func as (id, cfg)) =
    let
        val lva = mkLiveOutSets (Cfg.map bbToGK cfg)
        val oldIfe = IfeGraph.mkGraph ()
        val newIfe = IfeGraph.mkGraph ()
        val vtr = Util.mkHt ()
        val colorBB = fn (id, ins) => (id, List.map (colorIns vtr) ins)
    in
        (* Build the interference graph. *)
        Cfg.apply (bbIfeGraph oldIfe) lva;
        (* Build the vtr. *)
        List.app (addToIfe vtr newIfe) (deconstruct oldIfe);
        (* Map the old registers to the new ones. *)
        (id, Cfg.map colorBB cfg)
    end


fun regAlloc (PROGRAM {text=text, data=data}) =
    PROGRAM {text=List.map color text, data=data}

end
