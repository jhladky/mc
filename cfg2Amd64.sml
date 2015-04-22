open TargetAmd64;

signature CFG2AMD64 = sig
    val cfg2Amd64 : Cfg.program -> program;
end

structure Cfg2Amd64 :> CFG2AMD64 = struct

exception ILOCException of Iloc.opcode;

fun mkHt () = HashTable.mkTable (HashString.hashString, op =)
                                (10, Fail "Not Found")


val types : (string, int * (string, int) HashTable.hash_table)
                HashTable.hash_table = mkHt ();


fun rrr2Amd64 r1 r2 dest Iloc.OP_ADD =
    [INS_X {opcode=OP_RET}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_SUB =
    [INS_X {opcode=OP_RET}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_MULT =
    [INS_X {opcode=OP_RET}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_DIV =
    [INS_X {opcode=OP_RET}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_AND =
    [INS_X {opcode=OP_RET}]
  | rrr2Amd64 r1 r2 dest Iloc.OP_OR =
    [INS_X {opcode=OP_RET}]
  | rrr2Amd64 _ _ _ opcode = raise ILOCException opcode


fun rir2Amd64 r1 immed dest Iloc.OP_XORI =
    [INS_X {opcode=OP_RET}]
  | rir2Amd64 _ _ _ opcode = raise ILOCException opcode


fun rrc2Amd64 r1 r2 Iloc.OP_COMP =
    [INS_X {opcode=OP_RET}]
  | rrc2Amd64 _ _ opcode = raise ILOCException opcode


fun ric2Amd64 r1 immed Iloc.OP_COMPI =
    [INS_X {opcode=OP_RET}]
  | ric2Amd64 _ _ opcode = raise ILOCException opcode


fun rsr2Amd64 r1 field dest Iloc.OP_LOADAI =
    [INS_X {opcode=OP_RET}]
  | rsr2Amd64 _ _ _ opcode = raise ILOCException opcode


fun rrs2Amd64 r1 r2 field Iloc.OP_STOREAI =
    [INS_X {opcode=OP_RET}]
  | rrs2Amd64 _ _ _ opcode = raise ILOCException opcode


fun cll2Amd64 l1 l2 Iloc.OP_CBREQ =
    [INS_X {opcode=OP_RET}]
  | cll2Amd64 _ _ opcode = raise ILOCException opcode


fun sir2Amd64 r2 immed id Iloc.OP_LOADINARGUMENT =
    [INS_X {opcode=OP_RET}]
  | sir2Amd64 _ _ _ opcode = raise ILOCException opcode


fun new2Amd64 id fields Iloc.OP_NEW =
    [INS_X {opcode=OP_RET}]
  | new2Amd64 _ _ opcode = raise ILOCException opcode


fun rr2Amd64 r1 dest Iloc.OP_MOV =
    [INS_X {opcode=OP_RET}]
  | rr2Amd64 _ _ opcode = raise ILOCException opcode


fun ir2Amd64 immed dest Iloc.OP_LOADI =
    [INS_X {opcode=OP_RET}]
  | ir2Amd64 immed dest Iloc.OP_MOVEQ =
    [INS_X {opcode=OP_RET}]
  | ir2Amd64 immed dest Iloc.OP_MOVNE =
    [INS_X {opcode=OP_RET}]
  | ir2Amd64 immed dest Iloc.OP_MOVLT =
    [INS_X {opcode=OP_RET}]
  | ir2Amd64 immed dest Iloc.OP_MOVGT =
    [INS_X {opcode=OP_RET}]
  | ir2Amd64 immed dest Iloc.OP_MOVLE =
    [INS_X {opcode=OP_RET}]
  | ir2Amd64 immed dest Iloc.OP_MOVGE =
    [INS_X {opcode=OP_RET}]
  | ir2Amd64 _ _ opcode = raise ILOCException opcode


fun ri2Amd64 immed dest Iloc.OP_STOREOUTARGUMENT =
    [INS_X {opcode=OP_RET}]
  | ri2Amd64 _ _ opcode = raise ILOCException opcode


fun sr2Amd64 r1 id Iloc.OP_LOADGLOBAL =
    [INS_X {opcode=OP_RET}]
  | sr2Amd64 r1 id Iloc.OP_COMPUTEGLOBALADDRESS =
    [INS_X {opcode=OP_RET}]
  | sr2Amd64 _ _ opcode = raise ILOCException opcode


fun rs2Amd64 r1 id Iloc.OP_STOREGLOBAL =
    [INS_X {opcode=OP_RET}]
  | rs2Amd64 _ _ opcode = raise ILOCException opcode


fun l2Amd64 l1 Iloc.OP_JUMPI =
    [INS_L {opcode=OP_JMP, label=l1}]
  | l2Amd64 l1 Iloc.OP_CALL =
    [INS_L {opcode=OP_CALL, label=l1}]
  | l2Amd64 _ opcode = raise ILOCException opcode


fun r2Amd64 r1 Iloc.OP_LOADRET =
    [INS_X {opcode=OP_RET}]
  | r2Amd64 r1 Iloc.OP_STORERET =
    [INS_X {opcode=OP_RET}]
  | r2Amd64 r1 Iloc.OP_PRINT =
    [INS_X {opcode=OP_RET}]
  | r2Amd64 r1 Iloc.OP_PRINTLN =
    [INS_X {opcode=OP_RET}]
  | r2Amd64 r1 Iloc.OP_READ =
    [INS_X {opcode=OP_RET}]
  | r2Amd64 r1 Iloc.OP_DEL =
    [INS_X {opcode=OP_RET}]
  | r2Amd64 _ opcode = raise ILOCException opcode


fun x2Amd64 Iloc.OP_RET =
    [INS_X {opcode=OP_RET}]
  | x2Amd64 opcode = raise ILOCException opcode


val iloc2Amd64 =
 fn Iloc.INS_RRR {opcode=opc, dest=d, r1=r1, r2=r2}     => rrr2Amd64 r1 r2 d opc
  | Iloc.INS_RIR {opcode=opc, dest=d, r1=r1, immed=i}   => rir2Amd64 r1 i d opc
  | Iloc.INS_RRC {opcode=opc, r1=r1, r2=r2}             => rrc2Amd64 r1 r2 opc
  | Iloc.INS_RIC {opcode=opc, r1=r1, immed=i}           => ric2Amd64 r1 i opc
  | Iloc.INS_RSR {opcode=opc, dest=d, r1=r1, field=f}   => rsr2Amd64 r1 f d opc
  | Iloc.INS_RRS {opcode=opc, r1=r1, r2=r2, field=f}    => rrs2Amd64 r1 r2 f opc
  | Iloc.INS_CLL {opcode=opc, l1=l1, l2=l2}             => cll2Amd64 l1 l2 opc
  | Iloc.INS_SIR {opcode=opc, id=id, immed=i, r1=r1}    => sir2Amd64 r1 i id opc
  | Iloc.INS_NEW {opcode=opc, dest=_, id=id, fields=fs} => new2Amd64 id fs opc
  | Iloc.INS_RR  {opcode=opc, dest=d, r1=r1}            => rr2Amd64 r1 d opc
  | Iloc.INS_IR  {opcode=opc, dest=d, immed=i}          => ir2Amd64 i d opc
  | Iloc.INS_RI  {opcode=opc, dest=d, immed=i}          => ri2Amd64 i d opc
  | Iloc.INS_SR  {opcode=opc, id=id, r1=r1}             => sr2Amd64 r1 id opc
  | Iloc.INS_RS  {opcode=opc, id=id, r1=r1}             => rs2Amd64 r1 id opc
  | Iloc.INS_L   {opcode=opc, l1=l1}                    => l2Amd64 l1 opc
  | Iloc.INS_R   {opcode=opc, r1=r1}                    => r2Amd64 r1 opc
  | Iloc.INS_X   {opcode=opc}                           => x2Amd64 opc


(*This is the basic block level*)
fun bb2Amd64 (label, L) = (label, List.concat (map iloc2Amd64 L))


(*this is the function level*)
fun func2Amd64 (func as Cfg.FUNCTION {id=id, ...}) =
    (id, map bb2Amd64 (Cfg.toList func))


local
    fun addOffsets ht n [] = ht
      | addOffsets ht n ((Ast.VAR_DECL {id=id, ...})::xs) =
        (HashTable.insert ht (id, n); addOffsets ht (n + 1) xs)
in
    fun calcOffsets (Ast.TYPE_DECL {id=id, decls=decls, ...}) =
        HashTable.insert types (id, (length decls * 8,
                                     addOffsets (mkHt ()) 0 decls))
end


fun global2Amd64 (Ast.VAR_DECL {id=id, ...}) = id


fun cfg2Amd64 (Cfg.PROGRAM {funcs=funcs, types=types, decls=decls}) =
    (app calcOffsets types;
     PROGRAM {text=map func2Amd64 funcs, data=map global2Amd64 decls})
    handle ILOCException _ =>
           (print "Bad ILOC instruction.\n"; OS.Process.exit OS.Process.failure)

end
