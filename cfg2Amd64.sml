signature CFG2AMD64 = sig
    val cfg2Amd64 : SymbolTable.symbol_table -> Iloc.program ->
                    TargetAmd64.program
end

structure Cfg2Amd64 :> CFG2AMD64 = struct
open SymbolTable
open TargetAmd64

exception BadOpcode of Iloc.opcode


(* when you enter a function find out which one needs the most arguments
 * and then allocate at least that much space on the stack *)
fun genPrologue len =
    [INS_R {opcode=OP_PUSHQ, r1=REG_RBP},
     INS_RR {opcode=OP_MOVQ, r1=REG_RSP, r2=REG_RBP},
     INS_IR {opcode=OP_SUBQ, immed=len, r2=REG_RSP}]


fun genEpilogue len =
    [INS_IR {opcode=OP_ADDQ, immed=len, r2=REG_RSP},
     INS_RR {opcode=OP_MOVQ, r1=REG_RBP, r2=REG_RSP},
     INS_R {opcode=OP_POPQ, r1=REG_RBP},
     INS_X {opcode=OP_RET}]


fun rrr2Amd64 r1 r2 dest Iloc.OP_ADD =
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r2, r2=REG_N dest},
     INS_RR {opcode=OP_ADDQ, r1=REG_N r1, r2=REG_N dest}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_SUB = (*r1 - r2 => dest*)
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_N dest},
     INS_RR {opcode=OP_SUBQ, r1=REG_N r2, r2=REG_N dest}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_MULT =
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r2, r2=REG_N dest},
     INS_RR {opcode=OP_IMULQ, r1=REG_N r1, r2=REG_N dest}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_DIV =
    [INS_IR {opcode=OP_SARQ, immed=63, r2=REG_RDX},
     INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_RAX},
     INS_R {opcode=OP_IDIVQ, r1= REG_N r2},
     INS_RR {opcode=OP_MOVQ, r1=REG_RAX, r2=REG_N dest}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_AND =
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r2, r2=REG_N dest},
     INS_RR {opcode=OP_ANDQ, r1=REG_N r1, r2=REG_N dest}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_OR =
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r2, r2=REG_N dest},
     INS_RR {opcode=OP_ORQ, r1=REG_N r1, r2=REG_N dest}]
  | rrr2Amd64 _ _ _ opcode = raise BadOpcode opcode


fun rir2Amd64 r1 immed dest Iloc.OP_XORI =
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_N dest},
     INS_IR {opcode=OP_XORQ, immed=immed, r2=REG_N dest}]
  | rir2Amd64 r1 immed dest Iloc.OP_LOADAI =
    [INS_MR {opcode=OP_MOVQ, immed=immed * Util.WORD_SIZE, base=REG_N r1,
             offset=NONE, dest=REG_N dest}]
  | rir2Amd64 _ _ _ opcode = raise BadOpcode opcode


fun rri2Amd64 r1 r2 immed Iloc.OP_STOREAI =
    [INS_RM {opcode=OP_MOVQ, r1=REG_N r1, immed=immed * Util.WORD_SIZE,
             base=REG_N r2, offset=NONE}]
  | rri2Amd64 _ _ _ opcode = raise BadOpcode opcode


fun rrc2Amd64 r1 r2 Iloc.OP_COMP =
  [INS_RR {opcode=OP_CMP, r1=REG_N r1, r2=REG_N r2}]
  | rrc2Amd64 _ _ opcode = raise BadOpcode opcode


fun ric2Amd64 r1 immed Iloc.OP_COMPI =
  [INS_IR {opcode=OP_CMP, immed=immed, r2=REG_N r1}]
  | ric2Amd64 _ _ opcode = raise BadOpcode opcode


fun cll2Amd64 l1 l2 Iloc.OP_CBREQ =
    [INS_L {opcode=OP_JE, label=l1}, INS_L {opcode=OP_JMP, label=l2}]
  | cll2Amd64 _ _ opcode = raise BadOpcode opcode


fun sir2Amd64 r2 immed id Iloc.OP_LOADINARGUMENT =
    (case immed of
         0 => [INS_RR {opcode=OP_MOVQ, r1=REG_RDI, r2=REG_N r2}]
       | 1 => [INS_RR {opcode=OP_MOVQ, r1=REG_RSI, r2=REG_N r2}]
       | 2 => [INS_RR {opcode=OP_MOVQ, r1=REG_RDX, r2=REG_N r2}]
       | 3 => [INS_RR {opcode=OP_MOVQ, r1=REG_RCX, r2=REG_N r2}]
       | 4 => [INS_RR {opcode=OP_MOVQ, r1=REG_N 8, r2=REG_N r2}]
       | 5 => [INS_RR {opcode=OP_MOVQ, r1=REG_N 9, r2=REG_N r2}]
       | n => [INS_MR {opcode=OP_MOVQ, immed=Util.WORD_SIZE * (n - 6) + 16,
                       dest=REG_N r2,  base=REG_RBP, offset=NONE}])
  | sir2Amd64 _ _ _ opcode = raise BadOpcode opcode


fun new2Amd64 id fields dest Iloc.OP_NEW =
    [INS_IR {opcode=OP_MOVQ, immed=length fields * Util.WORD_SIZE, r2=REG_RDI},
     INS_L {opcode=OP_CALL, label="malloc"},
     INS_RR {opcode=OP_MOVQ, r1=REG_RAX, r2=REG_N dest}]
  | new2Amd64 _ _ _ opcode = raise BadOpcode opcode


fun rr2Amd64 r1 dest Iloc.OP_MOV =
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_N dest}]
  | rr2Amd64 r1 dest Iloc.OP_MOVEQ =
    [INS_RR {opcode=OP_CMOVE, r1=REG_N r1, r2=REG_N dest}]
  | rr2Amd64 r1 dest Iloc.OP_MOVNE =
    [INS_RR {opcode=OP_CMOVNE, r1=REG_N r1, r2=REG_N dest}]
  | rr2Amd64 r1 dest Iloc.OP_MOVLT =
    [INS_RR {opcode=OP_CMOVL, r1=REG_N r1, r2=REG_N dest}]
  | rr2Amd64 r1 dest Iloc.OP_MOVGT =
    [INS_RR {opcode=OP_CMOVG, r1=REG_N r1, r2=REG_N dest}]
  | rr2Amd64 r1 dest Iloc.OP_MOVLE =
    [INS_RR {opcode=OP_CMOVLE, r1=REG_N r1, r2=REG_N dest}]
  | rr2Amd64 r1 dest Iloc.OP_MOVGE =
    [INS_RR {opcode=OP_CMOVGE, r1=REG_N r1, r2=REG_N dest}]
  | rr2Amd64 _ _ opcode = raise BadOpcode opcode


fun ir2Amd64 immed dest Iloc.OP_LOADI =
    [INS_IR {opcode=OP_MOVQ, immed=immed, r2=REG_N dest}]
  | ir2Amd64 _ _ opcode = raise BadOpcode opcode


fun ri2Amd64 immed r1 Iloc.OP_STOREOUTARGUMENT =
    (case immed of
         0 => [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_RDI}]
       | 1 => [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_RSI}]
       | 2 => [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_RDX}]
       | 3 => [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_RCX}]
       | 4 => [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_N 8}]
       | 5 => [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_N 9}]
       | n => [INS_RM {opcode=OP_MOVQ, immed=Util.WORD_SIZE * (n - 6),
                       r1=REG_N r1, base=REG_RSP, offset=NONE}])
  | ri2Amd64 _ _ opcode = raise BadOpcode opcode


fun sr2Amd64 r1 id Iloc.OP_LOADGLOBAL =
    [INS_GR {opcode=OP_MOVQ, global=id, dest=REG_N r1}]
  | sr2Amd64 r1 id Iloc.OP_COMPUTEGLOBALADDRESS =
    [INS_SR {opcode=OP_MOVQ, id=id, dest=REG_N r1}]
  | sr2Amd64 _ _ opcode = raise BadOpcode opcode


fun rs2Amd64 r1 id Iloc.OP_STOREGLOBAL =
    [INS_RG {opcode=OP_MOVQ, r1=REG_N r1, global=id}]
  | rs2Amd64 _ _ opcode = raise BadOpcode opcode


fun l2Amd64 l1 Iloc.OP_JUMPI = [INS_L {opcode=OP_JMP, label=l1}]
  | l2Amd64 l1 Iloc.OP_CALL = [INS_L {opcode=OP_CALL, label=l1}]
  | l2Amd64 _ opcode = raise BadOpcode opcode


fun io2Amd64 r1 label funcName =
    [INS_SR {opcode=OP_MOVQ, id=label, dest=REG_RDI},
     INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_RSI},
     INS_IR {opcode=OP_MOVQ, immed=0, r2=REG_RAX},
     INS_L {opcode=OP_CALL, label=funcName}]


fun r2Amd64 r1 Iloc.OP_LOADRET =
    [INS_RR {opcode=OP_MOVQ, r1=REG_RAX, r2=REG_N r1}]
  | r2Amd64 r1 Iloc.OP_STORERET =
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_RAX}]
  | r2Amd64 r1 Iloc.OP_PRINT = io2Amd64 r1 "L__s__" "printf"
  | r2Amd64 r1 Iloc.OP_PRINTLN = io2Amd64 r1 "L__sn__" "printf"
  | r2Amd64 r1 Iloc.OP_READ = io2Amd64 r1 "L__s__" "scanf"
  | r2Amd64 r1 Iloc.OP_DEL =
    [INS_RR {opcode=OP_MOVQ, r1=REG_N r1, r2=REG_RDI},
     INS_L {opcode=OP_CALL, label="free"}]
  | r2Amd64 _ opcode = raise BadOpcode opcode


fun x2Amd64 len Iloc.OP_RET = genEpilogue len
  | x2Amd64 _ opcode = raise BadOpcode opcode


fun iloc2Amd64 len =
 fn Iloc.INS_RRR {opcode=opc, r1=r1, dest=d, r2=r2}     => rrr2Amd64 r1 r2 d opc
  | Iloc.INS_RIR {opcode=opc, r1=r1, dest=d, immed=i}   => rir2Amd64 r1 i d opc
  | Iloc.INS_RRI {opcode=opc, r1=r1, r2=r2, immed=i}    => rri2Amd64 r1 r2 i opc
  | Iloc.INS_RRC {opcode=opc, r1=r1, r2=r2}             => rrc2Amd64 r1 r2 opc
  | Iloc.INS_RIC {opcode=opc, r1=r1, immed=i}           => ric2Amd64 r1 i opc
  | Iloc.INS_SIR {opcode=opc, r1=r1, id=id, immed=i}    => sir2Amd64 r1 i id opc
  | Iloc.INS_CLL {opcode=opc, l1=l1, l2=l2}             => cll2Amd64 l1 l2 opc
  | Iloc.INS_NEW {opcode=opc, dest=d, id=id, fields=fs} => new2Amd64 id fs d opc
  | Iloc.INS_IR  {opcode=opc, dest=d, immed=i}          => ir2Amd64 i d opc
  | Iloc.INS_RI  {opcode=opc, dest=d, immed=i}          => ri2Amd64 i d opc
  | Iloc.INS_RR  {opcode=opc, r1=r1, dest=d}            => rr2Amd64 r1 d opc
  | Iloc.INS_SR  {opcode=opc, r1=r1, id=id}             => sr2Amd64 r1 id opc
  | Iloc.INS_RS  {opcode=opc, r1=r1, id=id}             => rs2Amd64 r1 id opc
  | Iloc.INS_R   {opcode=opc, r1=r1}                    => r2Amd64 r1 opc
  | Iloc.INS_L   {opcode=opc, l1=l1}                    => l2Amd64 l1 opc
  | Iloc.INS_X   {opcode=opc}                           => x2Amd64 len opc


fun getPLen funcs (call, max) =
    let
        val FUNC_INFO {params=ps, ...} = HashTable.lookup funcs call
        val new = length ps
    in
        if new > max then new else max
    end


(* Id is the function name, we need it so we know where the entry block is. *)
fun bb2Amd64 id len (l, L) =
    if l = id then (l, genPrologue len @ List.concat (map (iloc2Amd64 len) L))
    else (l, List.concat (map (iloc2Amd64 len) L))


(* This is the function level. *)
fun func2Amd64 (ST {funcs=funcs, ...}) (id, cfg) =
    let
        val (FUNC_INFO {calls=calls, ...}) = HashTable.lookup funcs id
        val max = foldr (getPLen funcs) 0 calls * Util.WORD_SIZE
    in
        (id, Cfg.map (bb2Amd64 id max) cfg)
    end


fun removeFuncs funcs id = not (List.exists (fn (fname, _) => fname = id) funcs)

(*we have a list of *)
fun cfg2Amd64 (st as ST {globals=globals, ...}) funcs =
    PROGRAM {
        text=map (func2Amd64 st) funcs,
        data=List.filter (removeFuncs funcs)
                         (map #1 (HashTable.listItemsi globals))
    }
    handle BadOpcode _ =>
           (print "Bad ILOC instruction.\n"; OS.Process.exit OS.Process.failure)

end
