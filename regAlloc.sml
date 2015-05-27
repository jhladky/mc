signature REG_ALLOC = sig
    val regAlloc : TargetAmd64.program -> TargetAmd64.program
end

structure RegAlloc :> REG_ALLOC = struct
open TargetAmd64
open UnorderedSet


datatype live_variable_analysis =
         LVA of {
             id: string,
             ins: instruction list,
             gk: register set * register set,
             liveOut: register set,
             diff: bool
         }

(* Set of all the actual i.e. not virtual, registers. *)
val actual = addList (empty (), [REG_RAX, REG_RBX, REG_RCX, REG_RDX,
                                 REG_RSI, REG_RDI, REG_RBP, REG_RSP,
                                 REG_8, REG_9, REG_10, REG_11,
                                 REG_12, REG_13, REG_14, REG_15])

(* Set of all the registers we have available for coloring. *)
val avail = addList (empty (), [REG_RBX, REG_RDX, REG_RSI, REG_12,
                                REG_13, REG_14, REG_15, REG_RCX,
                                REG_8, REG_9, REG_10, REG_11,
                                REG_RDI, REG_RBP, REG_RAX])

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


fun getNode ife reg =
    case IfeGraph.find ife reg of
        SOME node => node
      | NONE => IfeGraph.mkNode ife reg


fun addEdge ife r1 r2 = IfeGraph.addEdge (getNode ife r1) (getNode ife r2)


fun addEdgeNoCreate ife node reg =
    case IfeGraph.find ife reg of
        SOME other => IfeGraph.addEdge node other
      | NONE => ()


fun regToGK (ins, (gen, kill)) =
    let
        val (sources, targets) = getST ins
    in
        (condAddList (gen, kill) sources, addList (kill, targets))
    end


fun bbToGK (id, ins) =
    let
        val gk = List.foldl regToGK (empty (), empty ()) ins
    in
        LVA {id=id, ins=ins, gk=gk, liveOut=empty (), diff=true}
    end


fun bbLiveOut1 (LVA {gk=(gen, kill), liveOut=liveOut, ...}, s) =
    union (s, union (gen, difference (liveOut, kill)))


fun bbLiveOut node =
    let
        val LVA {id=id, ins=ins, gk=gk, liveOut=liveOut, ...} = Cfg.getData node
        val lo = List.foldr bbLiveOut1 (empty ()) (Cfg.getSuccs node)
        val diff = not (equal (liveOut, lo))
    in
        LVA {id=id, ins=ins, gk=gk, liveOut=lo, diff=diff}
    end


fun insIfeGraph ife (ins, liveNow) =
    let
        val (sources, targets) = getST ins
    in
        List.app (fn t => app (addEdge ife t) liveNow) targets;
        addList (List.foldr (fn (t, ln) => delete (ln, t) handle NotFound => ln)
                            liveNow targets, sources)
    end


fun bbIfeGraph ife node =
    let
        val lva as LVA {ins=ins, liveOut=liveOut, ...} = Cfg.getData node
    in
        List.foldr (insIfeGraph ife) liveOut ins;
        lva
    end


fun uncReq (reg, adjs) = length adjs < numItems avail andalso
                         not (member (actual, reg))


fun conReq (reg, adjs) = length adjs >= numItems avail andalso
                         not (member (actual, reg))


(* TODO: Make this much better, by actually using the heuristic and by
 * reanalyzing the graph every time.*)
fun deconstruct ife =
    (* Right now we need to get the adjacency information for each node. *)
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
    listItems (intersection (addList (empty (), HashTable.listItems vtr),
                             preserved))


fun colorCall vtr label =
    let
        val save = intersection (addList (empty (), HashTable.listItems vtr),
                                 scratch)
        val save = listItems save
        (* At this point the stack is already aligned. *)
        val doAlign = (length save) mod 2 <> 0
    in
        List.map (fn reg => INS_R {opcode=OP_PUSHQ, r1=reg}) save
        @ (if doAlign
           then [INS_IR {opcode=OP_SUBQ, immed=Util.WORD_SIZE, r2=REG_RSP}]
           else [])
        @ [INS_L {opcode=OP_CALL, label=label}]
        @ (if doAlign
           then [INS_IR {opcode=OP_ADDQ, immed=Util.WORD_SIZE, r2=REG_RSP}]
           else [])
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


fun diffCheck (LVA {diff=d, ...}, diff) = if d then true else diff


fun buildLvas cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app bbLiveOut cfg; buildLvas cfg)
    else cfg


(* calculate how much spill space we need. Make sure it is 16-byte aligned.
 * Check the existing amount of space as well as how many registers we have to
 * save at the top.*)
fun calcStackOffset existing vtr n =
    let
        val numSaves = length (getSaveList vtr) + 1
        val new = existing div Util.WORD_SIZE + n
    in
        (* Then branch: we might need to align the stack. *)
        if numSaves mod 2 = 0 then (if new mod 2 = 0 then new + 1
                                    else new) * Util.WORD_SIZE
        (* Else branch: we don't need to align the stack. *)
        else new * Util.WORD_SIZE
    end


(* Update the top-of-stack modifying instructions to make sure we
 * reserved enough spill space. *)
fun updatePP vtr n (INS_IR {opcode=OP_SUBQ, r2=REG_RSP, immed=immed}) =
    INS_IR {opcode=OP_SUBQ, r2=REG_RSP, immed=calcStackOffset immed vtr n}
  | updatePP vtr n (INS_IR {opcode=OP_ADDQ, r2=REG_RSP, immed=immed}) =
    INS_IR {opcode=OP_ADDQ, r2=REG_RSP, immed=calcStackOffset immed vtr n}
  | updatePP _ _ ins = ins


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
        (id, List.map (updatePP vtr n) L)
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


(* Build the interference graph. Then build the vtr. If we can color the
 * registers then map them the old ones, otherwise spill and try again.
 * n is the number of times we've had to spill. *)
fun color n (id, cfg) =
    let
        val lvas = buildLvas (Cfg.map bbToGK cfg)
        val oldIfe = IfeGraph.mkGraph ()
        val newIfe = IfeGraph.mkGraph ()
        val vtr = Util.mkHt ()
    in
        Cfg.app (bbIfeGraph oldIfe) lvas;
        case buildVtr (addToIfe vtr newIfe) (deconstruct oldIfe) of
            SOME reg => color (n + 1) (id, Cfg.map (spill n reg) cfg)
          | NONE => (id, Cfg.map (colorBB n id vtr) cfg)
    end


fun regAlloc (PROGRAM {text=text, data=data}) =
    PROGRAM {text=List.map (fn func => color 0 func) text, data=data}

end
