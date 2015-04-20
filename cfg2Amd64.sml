open TargetAmd64;

signature CFG2AMD64 = sig
    val cfg2Amd64 : (string, Cfg.cfg) HashTable.hash_table -> instruction list;
end

structure Cfg2Amd64 : CFG2AMD64 = struct

exception ILOCException of Iloc.opcode;

fun rrr2Amd64 r1 r2 dest Iloc.OP_ADD =
    INS_LABEL ""
  | rrr2Amd64 r1 r2 dest Iloc.OP_SUB =
    INS_LABEL ""
  | rrr2Amd64 r1 r2 dest Iloc.OP_MULT =
    INS_LABEL ""
  | rrr2Amd64 r1 r2 dest Iloc.OP_DIV =
    INS_LABEL ""
  | rrr2Amd64 r1 r2 dest Iloc.OP_AND =
    INS_LABEL ""
  | rrr2Amd64 r1 r2 dest Iloc.OP_OR =
    INS_LABEL ""
  | rrr2Amd64 _ _ _ opcode = raise ILOCException opcode


fun rir2Amd64 r1 immed dest Iloc.OP_XORI =
    INS_LABEL ""
  | rir2Amd64 _ _ _ opcode = raise ILOCException opcode


fun rrc2Amd64 r1 r2 Iloc.OP_COMP =
    INS_LABEL ""
  | rrc2Amd64 _ _ opcode = raise ILOCException opcode


fun ric2Amd64 r1 immed Iloc.OP_COMPI =
    INS_LABEL ""
  | ric2Amd64 _ _ opcode = raise ILOCException opcode


fun rsr2Amd64 r1 field dest Iloc.OP_LOADAI =
    INS_LABEL ""
  | rsr2Amd64 _ _ _ opcode = raise ILOCException opcode


fun rrs2Amd64 r1 r2 field Iloc.OP_STOREAI =
    INS_LABEL ""
  | rrs2Amd64 _ _ _ opcode = raise ILOCException opcode


fun cll2Amd64 l1 l2 Iloc.OP_CBREQ =
    INS_LABEL ""
  | cll2Amd64 _ _ opcode = raise ILOCException opcode


fun sir2Amd64 r2 immed id Iloc.OP_LOADINARGUMENT =
    INS_LABEL ""
  | sir2Amd64 _ _ _ opcode = raise ILOCException opcode


fun new2Amd64 id fields Iloc.OP_NEW =
    INS_LABEL ""
  | new2Amd64 _ _ opcode = raise ILOCException opcode


fun rr2Amd64 r1 dest Iloc.OP_MOV =
    INS_LABEL ""
  | rr2Amd64 _ _ opcode = raise ILOCException opcode


fun ir2Amd64 immed dest Iloc.OP_LOADI =
    INS_LABEL ""
  | ir2Amd64 immed dest Iloc.OP_MOVEQ =
    INS_LABEL ""
  | ir2Amd64 immed dest Iloc.OP_MOVNE =
    INS_LABEL ""
  | ir2Amd64 immed dest Iloc.OP_MOVLT =
    INS_LABEL ""
  | ir2Amd64 immed dest Iloc.OP_MOVGT =
    INS_LABEL ""
  | ir2Amd64 immed dest Iloc.OP_MOVLE =
    INS_LABEL ""
  | ir2Amd64 immed dest Iloc.OP_MOVGE =
    INS_LABEL ""
  | ir2Amd64 _ _ opcode = raise ILOCException opcode



fun ri2Amd64 immed dest Iloc.OP_STOREOUTARGUMENT =
    INS_LABEL ""
  | ri2Amd64 _ _ opcode = raise ILOCException opcode


fun sr2Amd64 r1 id Iloc.OP_LOADGLOBAL =
    INS_LABEL ""
  | sr2Amd64 r1 id Iloc.OP_COMPUTEGLOBALADDRESS =
    INS_LABEL ""
  | sr2Amd64 _ _ opcode = raise ILOCException opcode


fun rs2Amd64 r1 id Iloc.OP_STOREGLOBAL =
    INS_LABEL ""
  | rs2Amd64 _ _ opcode = raise ILOCException opcode


fun l2Amd64 l1 Iloc.OP_JUMPI =
    INS_LABEL ""
  | l2Amd64 l1 Iloc.OP_CALL =
    INS_LABEL ""
  | l2Amd64 _ opcode = raise ILOCException opcode


fun r2Amd64 r1 Iloc.OP_LOADRET =
    INS_LABEL ""
  | r2Amd64 r1 Iloc.OP_STORERET =
    INS_LABEL ""
  | r2Amd64 r1 Iloc.OP_PRINT =
    INS_LABEL ""
  | r2Amd64 r1 Iloc.OP_PRINTLN =
    INS_LABEL ""
  | r2Amd64 r1 Iloc.OP_READ =
    INS_LABEL ""
  | r2Amd64 r1 Iloc.OP_DEL =
    INS_LABEL ""
  | r2Amd64 _ opcode = raise ILOCException opcode


fun x2Amd64 Iloc.OP_RET =
    INS_LABEL ""
  | x2Amd64 opcode = raise ILOCException opcode


fun iloc2Amd64 (Iloc.INS_RRR {opcode=opcode, dest=dest, r1=r1, r2=r2}) =
    rrr2Amd64 r1 r2 dest opcode
  | iloc2Amd64 (Iloc.INS_RIR {opcode=opcode, dest=dest, r1=r1, immed=immed}) =
    rir2Amd64 r1 immed dest opcode
  | iloc2Amd64 (Iloc.INS_RRC {opcode=opcode, r1=r1, r2=r2}) =
    rrc2Amd64 r1 r2 opcode
  | iloc2Amd64 (Iloc.INS_RIC {opcode=opcode, r1=r1, immed=immed}) =
    ric2Amd64 r1 immed opcode
  | iloc2Amd64 (Iloc.INS_RSR {opcode=opcode, dest=dest, r1=r1, field=field}) =
    rsr2Amd64 r1 field dest opcode
  | iloc2Amd64 (Iloc.INS_RRS {opcode=opcode, r1=r1, r2=r2, field=field}) =
    rrs2Amd64 r1 r2 field opcode
  | iloc2Amd64 (Iloc.INS_CLL {opcode=opcode, l1=l1, l2=l2}) =
    cll2Amd64 l1 l2 opcode
  | iloc2Amd64 (Iloc.INS_SIR {opcode=opcode, id=id, immed=immed, r1=r1}) =
    sir2Amd64 r1 immed id opcode
  | iloc2Amd64 (Iloc.INS_NEW {opcode=opcode, dest=dest, id=id, fields=fields}) =
    new2Amd64 id fields opcode
  | iloc2Amd64 (Iloc.INS_RR {opcode=opcode, dest=dest, r1=r1}) =
    rr2Amd64 r1 dest opcode
  | iloc2Amd64 (Iloc.INS_IR {opcode=opcode, dest=dest, immed=immed}) =
    ir2Amd64 immed dest opcode
  | iloc2Amd64 (Iloc.INS_RI {opcode=opcode, dest=dest, immed=immed}) =
    ri2Amd64 immed dest opcode
  | iloc2Amd64 (Iloc.INS_SR {opcode=opcode, id=id, r1=r1}) =
    sr2Amd64 r1 id opcode
  | iloc2Amd64 (Iloc.INS_RS {opcode=opcode, id=id, r1=r1}) =
    rs2Amd64 r1 id opcode
  | iloc2Amd64 (Iloc.INS_L {opcode=opcode, l1=l1}) =
    l2Amd64 l1 opcode
  | iloc2Amd64 (Iloc.INS_R {opcode=opcode, r1=r1}) =
    r2Amd64 r1 opcode
  | iloc2Amd64 (Iloc.INS_X {opcode=opcode}) =
    x2Amd64 opcode


fun bb2Amd64 (id, bb) =
    [INS_LABEL id] @ map iloc2Amd64 bb

fun bbs2Amd64 bbs =
    foldl (fn (bb, L) => L @ bb2Amd64 bb) [] bbs

fun cfg2Amd64 ht =
    HashTable.fold (fn (cfg, L) => L @ bbs2Amd64 (Cfg.toList cfg)) [] ht
    handle ILOCException _ =>
           (print "Bad ILOC instruction.\n"; OS.Process.exit OS.Process.failure)

end
