open Ast;
open Iloc;

signature AST2CFG = sig
    val ast2Cfg : program -> (string, Cfg.cfg) HashTable.hash_table;
end

structure Ast2Cfg :> AST2CFG = struct

fun mkHt () = HashTable.mkTable (HashString.hashString, op =)
                                (10, Fail "Not Found")


val funcs : (string, Cfg.cfg) HashTable.hash_table = mkHt ();
val types : (string, string list) HashTable.hash_table = mkHt ();


fun idExpr2Ins cfg id =
    case HashTable.find (Cfg.getRegs cfg) id of
        SOME dest => (dest, [])
      | NONE =>
        let
            val dest = Cfg.nextReg cfg;
        in
            (dest, [INS_SR {opcode=OP_LOADGLOBAL, id=id, r1=dest}])
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


local
    fun dests2Stores1 n [] = []
      | dests2Stores1 n (dest::dests) =
        (dests2Stores1 (n + 1) dests) @
        [INS_RI {opcode=OP_STOREOUTARGUMENT, immed=n, dest=dest}]
in
    fun dests2Stores L = dests2Stores1 0 L
end


fun genLoad n dest = (dest, [INS_IR {opcode=OP_LOADI, immed=n, dest=dest}])


fun genJump node = [INS_L {opcode=OP_JUMPI, l1=Cfg.getLabel node}]


fun binExpr2Ins cfg opr lft rht =
    let
        val (rLft, lLft) = expr2Ins cfg lft;
        val (rRht, lRht) = expr2Ins cfg rht;
        val dest = Cfg.nextReg cfg;
    in
        if opr = BOP_PLUS orelse
           opr = BOP_MINUS orelse
           opr = BOP_TIMES orelse
           opr = BOP_DIVIDE orelse
           opr = BOP_AND orelse
           opr = BOP_OR
        then
            (dest,
             [INS_RRR {opcode=bop2Op opr, r1=rLft, r2=rRht, dest=dest}]
             @ lRht
             @ lLft)
        else
            (dest,
             [INS_IR {opcode=bop2Op opr, immed=1, dest=dest},
              INS_RRC {opcode=OP_COMP, r1=rLft, r2=rRht},
              INS_IR {opcode=OP_LOADI, immed=0, dest=dest}]
             @ lRht
             @ lLft)
    end


and unExpr2Ins cfg opnd UOP_NOT =
    let
        val (r1, L) = expr2Ins cfg opnd;
        val dest = Cfg.nextReg cfg;
    in
        (dest, INS_RIR {opcode=OP_XORI, immed=1, r1=r1, dest=dest}::L)
    end
  | unExpr2Ins cfg opnd UOP_MINUS =
    let
        val (r1, L) = expr2Ins cfg opnd;
        val dest1 = Cfg.nextReg cfg;
        val dest2 = Cfg.nextReg cfg;
    in
        (dest2,
         INS_RRR {opcode=OP_SUB, r1=dest1, r2=r1, dest=dest2}
         ::INS_IR {opcode=OP_LOADI, immed=0, dest=dest1}
         ::L)
    end


and expr2Ins cfg (EXP_NUM {value=value, ...}) = genLoad value (Cfg.nextReg cfg)
  | expr2Ins cfg (EXP_ID {id=id, ...}) = idExpr2Ins cfg id
  | expr2Ins cfg (EXP_TRUE {...}) = genLoad 1 (Cfg.nextReg cfg)
  | expr2Ins cfg (EXP_FALSE {...}) = genLoad 0 (Cfg.nextReg cfg)
  | expr2Ins cfg EXP_NULL = genLoad 0 (Cfg.nextReg cfg)
  | expr2Ins cfg (EXP_UNARY {opr=opr, opnd=opd, ...}) = unExpr2Ins cfg opd opr
  | expr2Ins cfg (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    binExpr2Ins cfg opr lft rht
  | expr2Ins cfg (EXP_DOT {lft=lft, prop=prop, ...}) =
    let
        val (r1, L) = expr2Ins cfg lft;
        val dest = Cfg.nextReg cfg;
    in
        (dest, INS_RSR {opcode=OP_LOADAI, r1=r1, field=prop, dest=dest}::L)
    end
  | expr2Ins cfg (EXP_NEW {id=id, ...}) =
    let
        val fields = HashTable.lookup types id;
        val dest = Cfg.nextReg cfg;
    in
        (dest, [INS_NEW {opcode=OP_NEW, id=id, fields=fields, dest=dest}])
    end
  | expr2Ins cfg (EXP_INVOCATION {id=id, args=args, ...}) =
    let
        val (dests, Ls) = ListPair.unzip (map (fn a => expr2Ins cfg a) args);
        val dest = Cfg.nextReg cfg;
    in
        (dest,
         [INS_R {opcode=OP_LOADRET, r1=dest}, INS_L {opcode=OP_CALL, l1=id}]
         @ dests2Stores dests
         @ (List.concat o List.rev) Ls)
    end


fun loadLvalue cfg (LV_ID {id=id, ...}) =
    (case HashTable.find (Cfg.getRegs cfg) id of
         SOME dest => (dest, [])
       | NONE =>
         let
             val dest = Cfg.nextReg cfg;
         in
             (dest, [INS_SR {opcode=OP_LOADGLOBAL, r1=dest, id=id}])
         end)
  | loadLvalue cfg (LV_DOT {lft=lft, prop=prop, ...}) =
    let
        val (r1, L) = loadLvalue cfg lft;
        val dest = Cfg.nextReg cfg;
    in
        (dest, L @ [INS_RSR {opcode=OP_LOADAI, r1=r1, field=prop, dest=dest}])
    end


fun lvalue2Ins cfg reg (LV_ID {id=id, ...}) =
    (case HashTable.find (Cfg.getRegs cfg) id of
         SOME dest => [INS_RR {opcode=OP_MOV, r1=reg, dest=dest}]
       | NONE => [INS_RS {opcode=OP_STOREGLOBAL, r1=reg, id=id}])
  | lvalue2Ins cfg reg (LV_DOT {lft=lft, prop=prop, ...}) =
    let
        val (r2, L) = loadLvalue cfg lft;
    in
        L @ [INS_RRS {opcode=OP_STOREAI, r1=reg, r2=r2, field=prop}]
    end


fun genBrnIns reg yes no =
    [INS_RIC {opcode=OP_COMPI, r1=reg, immed=1},
     INS_CLL {opcode=OP_CBREQ, l1=Cfg.getLabel yes, l2=Cfg.getLabel no}]


fun returnStmt2BB cfg node NONE =
    let
        val (exitNode, newNode) = Cfg.mkReturn cfg;
    in
        Cfg.fill node (genJump exitNode);
        Cfg.link node exitNode;
        newNode
    end
  | returnStmt2BB cfg node (SOME exp) =
    let
        val (exitNode, newNode) = Cfg.mkReturn cfg;
        val (dest, L) = expr2Ins cfg exp;
    in
        Cfg.fill node (List.rev L @ [INS_R {opcode=OP_STORERET, r1=dest}] @
                       genJump exitNode);
        Cfg.link node exitNode;
        newNode
    end


fun stmt2BB cfg node (ST_BLOCK stmts) =
    foldl (fn (s, node) => stmt2BB cfg node s) node stmts
  | stmt2BB cfg node (ST_ASSIGN {target=target, source=source, ...}) =
    let
        val (rX, L) = expr2Ins cfg source;
    in
        Cfg.fill node (List.rev L @ lvalue2Ins cfg rX target);
        node
    end
  | stmt2BB cfg node (ST_PRINT {body=body, endl=endl, ...}) =
    let
        val (dest, L) = expr2Ins cfg body;
        val opcode = if endl then OP_PRINTLN else OP_PRINT;
    in
        Cfg.fill node (List.rev (INS_R {opcode=opcode, r1=dest}::L));
        node
    end
  | stmt2BB cfg node (ST_READ {id=id, ...}) =
    let
        val dest = Cfg.nextReg cfg;
        val L = [INS_SR {opcode=OP_COMPUTEGLOBALADDRESS, id="rdest", r1=dest},
                 INS_R {opcode=OP_READ, r1=dest},
                 INS_SR {opcode=OP_LOADGLOBAL, id="rdest", r1=dest}]
                @ lvalue2Ins cfg dest id;
    in
        Cfg.fill node L;
        node
    end
  | stmt2BB cfg node (ST_IF {guard=guard, thenBlk=thenBlk,
                             elseBlk=elseBlk, ...}) =
    let
        val (thenNode, elseNode, exitNode) = Cfg.mkIf node;
        val thenResNode = stmt2BB cfg thenNode thenBlk;
        val elseResNode = stmt2BB cfg elseNode elseBlk;
        val (dest, L) = expr2Ins cfg guard;
    in
        Cfg.link elseResNode exitNode;
        Cfg.link thenResNode exitNode;
        Cfg.fill node (List.rev L @ genBrnIns dest thenNode elseNode);
        Cfg.fill thenResNode (genJump exitNode);
        Cfg.fill elseResNode (genJump exitNode);
        exitNode
    end
  | stmt2BB cfg node (ST_WHILE {guard=guard, body=body, ...}) =
    let
        val (guardNode, bodyNode, exitNode) = Cfg.mkWhile node;
        val bodyResNode = stmt2BB cfg bodyNode body;
        val (dest, L) = expr2Ins cfg guard;
    in
        Cfg.link bodyResNode guardNode;
        Cfg.fill guardNode (List.rev L @ genBrnIns dest bodyNode exitNode);
        Cfg.fill node (genJump guardNode);
        Cfg.fill bodyResNode (genJump guardNode);
        exitNode
    end
  | stmt2BB cfg node (ST_DELETE {exp=exp, ...}) =
    let
        val (dest, L) = expr2Ins cfg exp;
    in
        Cfg.fill node (List.rev L @ [INS_R {opcode=OP_DEL, r1=dest}]);
        node
    end
  | stmt2BB cfg node (ST_RETURN {exp=exp, ...}) =
    returnStmt2BB cfg node exp
  | stmt2BB cfg node (ST_INVOCATION {id=id, args=args, ...}) =
    let
        val (dests, Ls) = ListPair.unzip (map (fn a => expr2Ins cfg a) args);
    in
        Cfg.fill node (List.concat (map List.rev Ls)
                       @ List.rev (dests2Stores dests)
                       @ [INS_L {opcode=OP_CALL, l1=id}]);
        node
    end


fun mkFuncEntry n regs [] = []
  | mkFuncEntry n regs (VAR_DECL {id=id, ...}::xs) =
    INS_SIR {opcode=OP_LOADINARGUMENT, id=id,
             immed=n, r1=HashTable.lookup regs id}
    ::mkFuncEntry (n + 1) regs xs


fun func2Cfg (f as FUNCTION {id=id, body=body, params=params, ...}) =
    let
        val (entry, exit, cfg) = Cfg.mkCfg f;
        val _ = Cfg.fill entry (mkFuncEntry 0 (Cfg.getRegs cfg) params);
        val _ = Cfg.fill exit [INS_X {opcode=OP_RET}];
        val res = foldl (fn (s, node) => stmt2BB cfg node s) entry body;
    in
        Cfg.link res exit;
        Cfg.fill res (genJump exit);
        HashTable.insert funcs (id, cfg)
    end


fun addType (TYPE_DECL {id=id, decls=decls, ...}) =
    HashTable.insert types (id, map (fn (VAR_DECL {id=s, ...}) => s) decls)


fun ast2Cfg (PROGRAM {funcs=fs, types=ts, ...}) =
    (app addType ts; app func2Cfg fs; funcs)

end
