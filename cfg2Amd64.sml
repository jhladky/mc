open TargetAmd64;

signature CFG2AMD64 = sig
    val cfg2Amd64 : (string, Cfg.cfg) HashTable.hash_table -> instruction list;
end

structure Cfg2Amd64 : CFG2AMD64 = struct


fun rrr2Amd64 r1 r2 dest opcode =
    INS_LABEL ""


fun rir2Amd64 r1 immed dest opcode =
    INS_LABEL ""


fun rrc2Amd64 r1 r2 opcode =
    INS_LABEL ""


fun ric2Amd64 r1 immed opcode =
    INS_LABEL ""


fun rsr2Amd64 r1 field dest opcode =
    INS_LABEL ""


fun rrs2Amd64 r1 r2 field opcode =
    INS_LABEL ""


fun cll2Amd64 l1 l2 opcode =
    INS_LABEL ""


fun sir2Amd64 r2 immed id opcode =
    INS_LABEL ""


fun new2Amd64 id fields opcode =
    INS_LABEL ""


fun rr2Amd64 r1 dest opcode =
    INS_LABEL ""


fun ir2Amd64 immed dest opcode =
    INS_LABEL ""


fun ri2Amd64 immed dest opcode =
    INS_LABEL ""


fun sr2Amd64 r1 id opcode =
    INS_LABEL ""


fun rs2Amd64 r1 id opcode =
    INS_LABEL ""


fun si2Amd64 immed id opcode =
    INS_LABEL ""


fun l2Amd64 l1 opcode =
    INS_LABEL ""


fun r2Amd64 r1 opcode =
    INS_LABEL ""


fun x2Amd64 opcode =
    INS_LABEL ""


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
  | iloc2Amd64 (Iloc.INS_SI {opcode=opcode, id=id, immed=immed}) =
    si2Amd64 immed id opcode
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

end
