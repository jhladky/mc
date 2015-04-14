open Iloc;

signature AST2CFG = sig
    val ast2Cfg : program -> (string, Cfg.cfg) HashTable.hash_table;
end

structure Ast2Cfg :> AST2CFG = struct

fun mkHt () = HashTable.mkTable (HashString.hashString, op =)
                                (10, Fail "Not Found");


(* val globals : (string, miniType) HashTable.hash_table = mkHt (); *)
val funcs : (string, Cfg.cfg) HashTable.hash_table = mkHt ();
val types : (string, string list) HashTable.hash_table = mkHt ();


fun idExpr2Ins f L id =
    let
        val regs = Cfg.getRegs (HashTable.lookup funcs f);
    in
        case HashTable.find regs id of
            SOME dest => (dest, L)
          | NONE =>
            let
                val dest = Cfg.nextReg (HashTable.lookup funcs f);
            in
                (dest,
                 INS_SR {opcode=OP_LOADGLOBAL, id=id, r1=dest}::L)
            end
    end


fun bop2Op BOP_PLUS = OP_ADD
   | bop2Op BOP_MINUS = OP_SUB
   | bop2Op BOP_TIMES = OP_MULT
   | bop2Op BOP_DIVIDE = OP_DIV
   | bop2Op BOP_AND = OP_AND
   | bop2Op BOP_OR = OP_OR
   | bop2Op BOP_EQ = OP_MOVEQ
   | bop2Op BOP_NE = OP_MOVNE
   | bop2Op BOP_LT = OP_MOVLT
   | bop2Op BOP_GT = OP_MOVGT
   | bop2Op BOP_LE = OP_MOVLE
   | bop2Op BOP_GE = OP_MOVGE


fun binExpr2Ins f L opr lft rht =
    let
        val (rX, L1) = expr2Ins f L lft;
        val (rY, L2) = expr2Ins f L1 rht;
        val dest = Cfg.nextReg (HashTable.lookup funcs f);
    in
        if opr = BOP_PLUS orelse
           opr = BOP_MINUS orelse
           opr = BOP_TIMES orelse
           opr = BOP_DIVIDE orelse
           opr = BOP_AND orelse
           opr = BOP_OR
        then
            (dest,
             INS_RRR {opcode=bop2Op opr, r1=rX, r2=rY, dest=dest}::L)
        else
            (dest,
             INS_IR {opcode=bop2Op opr, immed=1, dest=dest}::
             INS_RRC {opcode=OP_COMP, r1=rX, r2=rY}::
             INS_IR {opcode=OP_LOADI, immed=0, dest=dest}::L)
    end


and unExpr2Ins f L opnd UOP_NOT =
    let
        val (dest1, L1) = expr2Ins f L opnd;
        val dest2 = Cfg.nextReg (HashTable.lookup funcs f);
    in
        (dest2,
         INS_RIR {opcode=OP_XORI, immed=0xFFFF, r1=dest1, dest=dest2}::L1)
    end
  | unExpr2Ins f L opnd UOP_MINUS =
    let
        val (dest1, L1) = expr2Ins f L opnd;
        val dest2 = Cfg.nextReg (HashTable.lookup funcs f);;
        val dest3 = Cfg.nextReg (HashTable.lookup funcs f);;
    in
        (dest3,
         INS_RRR {opcode=OP_SUB, r1=dest1, r2=dest2, dest=dest3}::
         INS_IR {opcode=OP_LOADI, immed=0, dest=dest2}::L1)
    end


and expr2Ins f L (EXP_NUM {value=value, ...}) =
    let
        val dest = Cfg.nextReg (HashTable.lookup funcs f);
    in
        (dest, INS_IR {opcode=OP_LOADI, immed=value, dest=dest}::L)
    end
  | expr2Ins f L (EXP_ID {id=id, ...}) = idExpr2Ins f L id
  | expr2Ins f L (EXP_TRUE {...}) =
    let
        val dest = Cfg.nextReg (HashTable.lookup funcs f);
    in
        (dest, INS_IR {opcode=OP_LOADI, immed=1, dest=dest}::L)
    end
  | expr2Ins f L (EXP_FALSE {...}) =
    let
        val dest = Cfg.nextReg (HashTable.lookup funcs f);
    in
        (dest, INS_IR {opcode=OP_LOADI, immed=0, dest=dest}::L)
    end
  | expr2Ins f L EXP_UNDEFINED =
    (1, L)
  | expr2Ins f L (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    binExpr2Ins f L opr lft rht
  | expr2Ins f L (EXP_UNARY {opr=opr, opnd=opnd, ...}) =
    unExpr2Ins f L opnd opr
  | expr2Ins f L (EXP_DOT {lft=lft, prop=prop, ...}) =
    (1, L)
  | expr2Ins f L (EXP_NEW {id=id, ...}) =
    let
        val dest = Cfg.nextReg (HashTable.lookup funcs f);
        val fields = HashTable.lookup types id;
    in
        (dest, INS_NEW {opcode=OP_NEW, id=id, fields=fields, dest=dest}::L)
    end
  | expr2Ins f L (EXP_INVOCATION {id=id, args=args, ...}) =
    (1, L)


(* CONVENTION: elseBlk is added first, then the thenBlk. So the thenBlk
 * will always be BEFORE the elseBlk in the list *)
fun stmt2BB f bb (ST_BLOCK stmts) =
    foldl (fn (s, bb) => stmt2BB f bb s) bb stmts
  | stmt2BB _ bb (ST_ASSIGN {target=target, source=source, ...}) =
    bb
  | stmt2BB f bb (ST_PRINT {body=body, endl=endl, ...}) =
    let
        val (dest, L) = expr2Ins f [] body;
        val opcode = if endl then OP_PRINTLN else OP_PRINT;
    in
        Cfg.fill bb (List.rev L);
        Cfg.fill bb [INS_R {opcode=opcode, r1=dest}];
        bb
    end
  | stmt2BB _ bb (ST_READ {id=id, ...}) =
    bb
  | stmt2BB f bb (ST_IF {guard=guard, thenBlk=thenBlk,
                         elseBlk=elseBlk, ...}) =
    let
        val exitBB = Cfg.mkNode ();
        val thenBB = Cfg.mkNode ();
        val elseBB = Cfg.mkNode ();
        val thenResBB = stmt2BB f thenBB thenBlk;
        val elseResBB = stmt2BB f elseBB elseBlk;
        val (dest, L) = expr2Ins f [] guard;
    in
        Cfg.link bb elseBB;
        Cfg.link bb thenBB;
        Cfg.link elseResBB exitBB;
        Cfg.link thenResBB exitBB;
        Cfg.fill bb (List.rev L);
        Cfg.fill bb [INS_RIC {opcode=OP_COMP, r1=dest, immed=1},
                     INS_CLL {opcode=OP_CBREQ, l1=Cfg.getLabel thenBB,
                              l2=Cfg.getLabel elseBB}];
        exitBB
    end
  | stmt2BB f bb (ST_WHILE {guard=guard, body=body, ...}) =
    let
        val guardBB = Cfg.mkNode ();
        val bodyBB = Cfg.mkNode ();
        val exitBB = Cfg.mkNode ();
        val bodyResBB = stmt2BB f bodyBB body;
        val (_, L) = expr2Ins f [] guard;
    in
        Cfg.fill guardBB (List.rev L);
        Cfg.link bb guardBB;
        Cfg.link guardBB bodyBB;
        Cfg.link guardBB exitBB;
        Cfg.link bodyResBB guardBB;
        exitBB
    end
  | stmt2BB f bb (ST_DELETE {exp=exp, ...}) =
    let
        val (dest, L) = expr2Ins f [] exp;
    in
        Cfg.fill bb ((List.rev L) @ [INS_R {opcode=OP_DEL, r1=dest}]);
        bb
    end
  | stmt2BB f bb (ST_RETURN {exp=exp, ...}) =
    let
        val exit = Cfg.getExit (HashTable.lookup funcs f)
    in
        Cfg.fill bb [INS_L {opcode=OP_JUMPI, l1=Cfg.getLabel exit}];
        Cfg.link bb exit;
        Cfg.mkNode ()
    end
  | stmt2BB _ bb (ST_INVOCATION {id=id, args=args, ...}) =
    bb


fun mkFuncEntry n regs [] = []
  | mkFuncEntry n regs (VAR_DECL {id=id, ...}::xs) =
    let
        val reg = HashTable.lookup regs id
    in
        INS_SIR {opcode=OP_LOADINARGUMENT, id=id, immed=n, r1=reg}::
        mkFuncEntry (n + 1) regs xs
    end


fun func2Cfg (f as FUNCTION {id=id, body=body, params=params, ...}) =
    let
        val (entryBB, exitBB, cfg) = Cfg.mkCfg f;
        val _ = HashTable.insert funcs (id, cfg);
        val _ = Cfg.fill entryBB (mkFuncEntry 0 (Cfg.getRegs cfg) params);
        val _ = Cfg.fill exitBB [INS_X {opcode=OP_RET}];
        val resBB = foldl (fn (s, bb) => stmt2BB id bb s) entryBB body;
    in
        Cfg.link resBB exitBB
    end


fun addType (TYPE_DECL {id=id, decls=decls, ...}) =
    HashTable.insert types (id, map (fn (VAR_DECL {id=s, ...}) => s) decls)


fun ast2Cfg (PROGRAM {funcs=fs, types=ts, ...}) =
    (app addType ts; app func2Cfg fs; funcs)

end
