signature REG_ALLOC = sig
    val regAlloc : TargetAmd64.program -> TargetAmd64.program
end

structure RegAlloc :> REG_ALLOC = struct
open Util
open Dfa
open TargetAmd64
open UnorderedSet


(* Set of all the registers we have available for coloring. *)
val avail = addList (empty (), [REG_RAX, REG_RBX, REG_RDX, REG_RSI, REG_12,
                                REG_13, REG_14, REG_15, REG_RCX, REG_8,
                                REG_9, REG_10, REG_11, REG_RDI, REG_RBP])

(* Registers that must be preserved in the function i.e. callee saved. *)
val preserved = addList (empty (), [REG_RBX, REG_RSP, REG_RBP, REG_12,
                                    REG_13, REG_14, REG_15])

(* Registers that can be used as scratch i.e. caller saved. *)
val scratch = addList (empty (), [REG_RAX, REG_RDI, REG_RSI, REG_RDX, REG_RCX,
                                  REG_8, REG_9, REG_10, REG_11])


fun isVirt (REG_V _) = true | isVirt _ = false


fun condAdd kill (reg, gen) =
    if not (member (kill, reg)) then add (gen, reg) else gen


fun condAddList (gen, kill) regs = foldr (condAdd kill) gen regs


fun iToGK (ins, (gen, kill)) =
    let
        val (sources, targets) = getST ins
    in
        (condAddList (gen, kill) sources, addList (kill, targets))
    end


fun insToGK (id, ins) = foldl iToGK (empty (), empty ()) ins


fun bbToDFA (id, ins) =
    DFA {id=id, ins=ins, gk=insToGK (id, ins), propSet=empty (), diff=true}


fun propagate1 (DFA {gk=(gen, kill), propSet=liveOut, ...}, s) =
    union (s, union (gen, difference (liveOut, kill)))


fun propagate node =
    let
        val DFA {id=id, ins=ins, gk=gk, propSet=liveOut, ...} = Cfg.getData node
        val lo = foldr propagate1 (empty ()) (Cfg.getSuccs node)
        val diff = not (equal (liveOut, lo))
    in
        DFA {id=id, ins=ins, gk=gk, propSet=lo, diff=diff}
    end


fun getNode ife reg =
    case IfeGraph.find ife reg of
        SOME node => node
      | NONE => IfeGraph.mkNode ife reg


fun addEdge ife r1 r2 =
    if r1 <> r2 then IfeGraph.addEdge (getNode ife r1) (getNode ife r2) else ()


fun insIfeGraph ife (i, liveNow) =
    let
        val (sources, targets) = getST i
    in
        List.app (fn t => app (addEdge ife t) liveNow) targets;
        addList (foldr (fn (t, ln) => delete (ln, t) handle NotFound => ln)
                       liveNow targets, sources)
    end


fun mkIfe (DFA {ins=ins, propSet=liveOut, ...}, ife) =
    (foldr (insIfeGraph ife) liveOut ins; ife)


fun deconstruct ife =
    let
        val (virt, real) = List.partition (isVirt o #1) (IfeGraph.toListRep ife)
        val f = fn ((_, adjs1), (_, adjs2)) => length adjs1 < length adjs2
    in
        real @ ListMergeSort.sort f virt
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
    difference (avail, foldr (addRealRegToSet vtr) (empty ()) adjs)


(* Return a bool indicating whether we have to spill
 * Here we're dealing with a virtual register.
 * `Pick` also returns the rest of the list, we don't use it. *)
fun buildVtr vtr [] = true
  | buildVtr vtr ((REG_V n, adjs)::nodes) =
    (case pick (getAvailRegs vtr adjs) of
         SOME (reg, _) =>
         (HashTable.insert vtr (Int.toString n, reg); buildVtr vtr nodes)
       | NONE => false)
  | buildVtr vtr (_::nodes) = buildVtr vtr nodes


fun replace vtr (REG_V n) = HashTable.lookup vtr (Int.toString n)
  | replace vtr reg = reg


fun replaceOffset vtr (SOME (reg, scalar)) = SOME (replace vtr reg, scalar)
  | replaceOffset vtr NONE = NONE


fun getSaveList vtr =
    listItems (intersection (addList (empty (), HashTable.listItems vtr),
                             preserved))


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
    [INS_L {opcode=OP_CALL, label=label}]
  | colorIns vtr (INS_X {opcode=OP_RET}) =
    List.rev (List.map (fn reg => INS_R {opcode=OP_POPQ, r1=reg})
                       (getSaveList vtr)) @ [INS_X {opcode=OP_RET}]
  | colorIns _ ins = [ins]


(* Calculate how much spill space we need. Make sure it is 16-byte aligned.
 * Check the existing amount of space as well as how many registers we have to
 * save at the top.*)
fun calcStackOffset existing vtr n =
    let
        val numSaves = length (getSaveList vtr) + 1 (* The +1 is for %rbp *)
        val new = existing div WORD_SIZE + n
    in
        (* Then branch: we might need to align the stack. *)
        if numSaves mod 2 = 0 then (if new mod 2 = 0 then new + 1
                                    else new) * WORD_SIZE
        (* Else branch: we don't need to align the stack. *)
        else new * WORD_SIZE
    end


(* Update the top-of-stack modifying instructions to make sure we
 * reserved enough spill space. *)
fun updatePP vtr n (INS_IR {opcode=OP_SUBQ, r2=REG_RSP, immed=immed}) =
    INS_IR {opcode=OP_SUBQ, r2=REG_RSP, immed=calcStackOffset immed vtr n}
  | updatePP vtr n (INS_IR {opcode=OP_ADDQ, r2=REG_RSP, immed=immed}) =
    INS_IR {opcode=OP_ADDQ, r2=REG_RSP, immed=calcStackOffset immed vtr n}
  | updatePP _ _ ins = ins


fun colorBB spilled funcId vtr (id, ins) =
    let
        val L = if id = funcId
                then List.map (fn reg => INS_R {opcode=OP_PUSHQ, r1=reg})
                              (getSaveList vtr)
                else []
        val L = L @ foldr (fn (ins, L) => colorIns vtr ins @ L) [] ins
        val n = length spilled
    in
        (id, List.map (updatePP vtr n) L)
    end


fun spillReg spilled reg ins =
    let
        val (sources, targets) = getST ins
        val n = ~((length spilled + 1) * WORD_SIZE)
    in
        (if has reg sources
         then [INS_MR {opcode=OP_MOVQ, immed=n, base=REG_RBP, dest=reg,
                       offset=NONE}]
         else [])
        @ [ins]
        @ (if has reg targets
           then [INS_RM {opcode=OP_MOVQ, immed=n, base=REG_RBP, offset=NONE,
                         r1=reg}]
           else [])
  end


fun spill spilled reg (id, ins) =
    (id, foldr (fn (ins, L) => spillReg spilled reg ins @ L) [] ins)


fun getSpillReg spilled [] = raise Fail "No spill candidate!"
  | getSpillReg spilled ((reg, adjs)::nodes) =
    if has reg spilled then getSpillReg spilled nodes else reg


(* Build the interference graph. Then build the vtr. If we can color the
 * registers then map them, otherwise spill and try again. *)
fun color spilled (id, cfg) =
    let
        val vtr = mkHt ()
        val ife = Cfg.fold mkIfe (IfeGraph.mkGraph ())
                           (buildDFAs propagate (Cfg.map bbToDFA cfg))
        val stack = deconstruct ife
    in
        if buildVtr vtr stack
        then (id, Cfg.map (colorBB spilled id vtr) cfg)
        else let
            val reg = getSpillReg spilled (List.filter (isVirt o #1) stack)
        in
            color (reg::spilled) (id, Cfg.map (spill spilled reg) cfg)
        end
    end


fun regAlloc (PROGRAM {text=text, data=data}) =
    PROGRAM {text=List.map (color []) text, data=data}

end
