signature AST2ILOC = sig
    val ast2Iloc : SymbolTable.symbol_table -> Ast.program -> Iloc.program
end

structure Ast2Iloc :> AST2ILOC = struct
open Ast
open Iloc
open IlocUtil

val types = Util.mkHt ()
val offsets = Util.mkHt ()


fun getOffset prop (MT_STRUCT r) =
    HashTable.lookup (HashTable.lookup offsets r) prop
  | getOffset _ _ = raise Fail ""


fun idExpr2Ins (ii as II {regs=regs, ...}) id =
    case HashTable.find regs id of
        SOME dest => (dest, [])
      | NONE =>
        let
            val dest = nextReg ii
        in
            (dest, [INS_SR {opcode=OP_LOADGLOBAL, id=id, r1=dest}])
        end


val bop2Op =
 fn BOP_PLUS => OP_ADD
  | BOP_MINUS => OP_SUB
  | BOP_TIMES => OP_MULT
  | BOP_DIVIDE => OP_DIV
  | BOP_AND => OP_AND
  | BOP_OR => OP_OR
  | BOP_EQ => OP_MOVEQ
  | BOP_NE => OP_MOVNE
  | BOP_LT => OP_MOVLT
  | BOP_GT => OP_MOVGT
  | BOP_LE => OP_MOVLE
  | BOP_GE => OP_MOVGE


local
    fun dests2Stores1 n [] = []
      | dests2Stores1 n (dest::dests) =
        dests2Stores1 (n + 1) dests
        @ [INS_RI {opcode=OP_STOREOUTARGUMENT, immed=n, dest=dest}]
in
    fun dests2Stores L = dests2Stores1 0 L
end


fun genLoad n dest = (dest, [INS_IR {opcode=OP_LOADI, immed=n, dest=dest}])


fun genJump node = [INS_L {opcode=OP_JUMPI, l1=getLabel node}]


fun binExpr2Ins ii opr lft rht =
    let
        val (rLft, lLft) = expr2Ins ii lft
        val (rRht, lRht) = expr2Ins ii rht
        val dest = nextReg ii
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


and unExpr2Ins ii opnd UOP_NOT =
    let
        val (r1, L) = expr2Ins ii opnd
        val dest = nextReg ii
    in
        (dest, INS_RIR {opcode=OP_XORI, immed=1, r1=r1, dest=dest}::L)
    end
  | unExpr2Ins ii opnd UOP_MINUS =
    let
        val (r1, L) = expr2Ins ii opnd
        val dest1 = nextReg ii
        val dest2 = nextReg ii
    in
        (dest2,
         INS_RRR {opcode=OP_SUB, r1=dest1, r2=r1, dest=dest2}
         ::INS_IR {opcode=OP_LOADI, immed=0, dest=dest1}
         ::L)
    end


and expr2Ins ii (EXP_NUM {value=value, ...}) = genLoad value (nextReg ii)
  | expr2Ins ii (EXP_ID {id=id, ...}) = idExpr2Ins ii id
  | expr2Ins ii (EXP_TRUE _) = genLoad 1 (nextReg ii)
  | expr2Ins ii (EXP_FALSE _) = genLoad 0 (nextReg ii)
  | expr2Ins ii (EXP_NULL _) = genLoad 0 (nextReg ii)
  | expr2Ins ii (EXP_UNARY {opr=opr, opnd=opd, ...}) = unExpr2Ins ii opd opr
  | expr2Ins ii (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    binExpr2Ins ii opr lft rht
  | expr2Ins ii (EXP_DOT {lft=lft, prop=prop, ...}) =
    let
        val (r1, L) = expr2Ins ii lft
        val dest = nextReg ii
        val (II {id=id, st=st, ...}) = ii
        val offset = getOffset prop (Static.getExprType id st lft)
    in
        (dest, INS_RIR {opcode=OP_LOADAI, r1=r1, immed=offset, dest=dest}::L)
    end
  | expr2Ins ii (EXP_NEW {id=id, ...}) =
    let
        val fields = HashTable.lookup types id
        val dest = nextReg ii
    in
        (dest, [INS_NEW {opcode=OP_NEW, id=id, fields=fields, dest=dest}])
    end
  | expr2Ins ii (EXP_INVOCATION {id=id, args=args, ...}) =
    let
        val (dests, Ls) = ListPair.unzip (map (fn a => expr2Ins ii a) args)
        val dest = nextReg ii
    in
        (dest,
         [INS_R {opcode=OP_LOADRET, r1=dest}, INS_L {opcode=OP_CALL, l1=id}]
         @ dests2Stores dests
         @ (List.concat o List.rev) Ls)
    end


fun loadLvalue (ii as II {regs=regs, ...}) (LV_ID {id=id, ...}) =
    (case HashTable.find regs id of
         SOME dest => (dest, [])
       | NONE =>
         let
             val dest = nextReg ii
         in
             (dest, [INS_SR {opcode=OP_LOADGLOBAL, r1=dest, id=id}])
         end)
  | loadLvalue ii (LV_DOT {lft=lft, prop=prop, ...}) =
    let
        val (r1, L) = loadLvalue ii lft
        val dest = nextReg ii
        val II {id=id, st=st, ...} = ii
        val offset = getOffset prop (Static.getLvalueType id st lft)
    in
        (dest, L @ [INS_RIR {opcode=OP_LOADAI, r1=r1, immed=offset, dest=dest}])
    end


fun lvalue2Ins (II {regs=regs, ...}) reg (LV_ID {id=id, ...}) =
    (case HashTable.find regs id of
         SOME dest => [INS_RR {opcode=OP_MOV, r1=reg, dest=dest}]
       | NONE => [INS_RS {opcode=OP_STOREGLOBAL, r1=reg, id=id}])
  | lvalue2Ins ii reg (LV_DOT {lft=lft, prop=prop, ...}) =
    let
        val (r2, L) = loadLvalue ii lft
        val II {id=id, st=st, ...} = ii
        val offset = getOffset prop (Static.getLvalueType id st lft)
    in
        L @ [INS_RRI {opcode=OP_STOREAI, r1=reg, r2=r2, immed=offset}]
    end


fun genBrnIns reg yes no =
    [INS_RIC {opcode=OP_COMPI, r1=reg, immed=1},
     INS_CLL {opcode=OP_CBREQ, l1=getLabel yes, l2=getLabel no}]


fun returnStmt2BB (II {cfg=cfg, ...}) node NONE =
    let
        val (exitNode, newNode) = mkReturn cfg
    in
        fill node (genJump exitNode);
        Cfg.link node exitNode;
        newNode
    end
  | returnStmt2BB (ii as II {cfg=cfg, ...}) node (SOME exp) =
    let
        val (exitNode, newNode) = mkReturn cfg
        val (dest, L) = expr2Ins ii exp
    in
        fill node (List.rev L
                   @ [INS_R {opcode=OP_STORERET, r1=dest}]
                   @ genJump exitNode);
        Cfg.link node exitNode;
        newNode
    end


fun stmt2BB ii node (ST_BLOCK stmts) =
    foldl (fn (s, node) => stmt2BB ii node s) node stmts
  | stmt2BB ii node (ST_ASSIGN {target=target, source=source, ...}) =
    let
        val (rX, L) = expr2Ins ii source
    in
        fill node (List.rev L @ lvalue2Ins ii rX target);
        node
    end
  | stmt2BB ii node (ST_PRINT {body=body, endl=endl, ...}) =
    let
        val (dest, L) = expr2Ins ii body
        val opcode = if endl then OP_PRINTLN else OP_PRINT
    in
        fill node (List.rev (INS_R {opcode=opcode, r1=dest}::L));
        node
    end
  | stmt2BB ii node (ST_READ {id=id, ...}) =
    let
        val dest = nextReg ii
        val L = [INS_SR {opcode=OP_COMPUTEGLOBALADDRESS, id="rdest", r1=dest},
                 INS_R {opcode=OP_READ, r1=dest},
                 INS_SR {opcode=OP_LOADGLOBAL, id="rdest", r1=dest}]
                @ lvalue2Ins ii dest id
    in
        fill node L;
        node
    end
  | stmt2BB ii node (ST_IF {guard=guard, thenBlk=thenBlk,
                            elseBlk=elseBlk, ...}) =
    let
        val (thenNode, elseNode, exitNode) = mkIf node
        val thenResNode = stmt2BB ii thenNode thenBlk
        val elseResNode = stmt2BB ii elseNode elseBlk
        val (dest, L) = expr2Ins ii guard
    in
        Cfg.link elseResNode exitNode;
        Cfg.link thenResNode exitNode;
        fill node (List.rev L @ genBrnIns dest thenNode elseNode);
        fill thenResNode (genJump exitNode);
        fill elseResNode (genJump exitNode);
        exitNode
    end
  | stmt2BB ii node (ST_WHILE {guard=guard, body=body, ...}) =
    let
        val (guardNode, bodyNode, exitNode) = mkWhile node
        val bodyResNode = stmt2BB ii bodyNode body
        val (dest, L) = expr2Ins ii guard
    in
        Cfg.link bodyResNode guardNode;
        fill guardNode (List.rev L @ genBrnIns dest bodyNode exitNode);
        fill node (genJump guardNode);
        fill bodyResNode (genJump guardNode);
        exitNode
    end
  | stmt2BB ii node (ST_DELETE {exp=exp, ...}) =
    let
        val (dest, L) = expr2Ins ii exp
    in
        fill node (List.rev L @ [INS_R {opcode=OP_DEL, r1=dest}]);
        node
    end
  | stmt2BB ii node (ST_RETURN {exp=exp, ...}) = returnStmt2BB ii node exp
  | stmt2BB ii node (ST_INVOCATION {id=id, args=args, ...}) =
    let
        val (dests, Ls) = ListPair.unzip (map (fn a => expr2Ins ii a) args)
    in
        fill node (List.concat (map List.rev Ls)
                   @ List.rev (dests2Stores dests)
                   @ [INS_L {opcode=OP_CALL, l1=id}]);
        node
    end


fun mkFuncEntry _ _ [] = []
  | mkFuncEntry n (ii as II {regs=regs, ...}) (VAR_DECL {id=id, ...}::xs) =
    INS_SIR {opcode=OP_LOADINARGUMENT, id=id,
             immed=n, r1=HashTable.lookup regs id}
    ::mkFuncEntry (n + 1) ii xs


fun func2Cfg st (func as FUNCTION {id=id, body=body, params=params, ...}) =
    let
        val (entry, exit, cfg) = Cfg.mkCfg (id, []) (nextLabel (), [])
        val ii = mkIi st cfg func
        val _ = fill entry (mkFuncEntry 0 ii params)
        val _ = fill exit [INS_X {opcode=OP_RET}]
        val res = foldl (fn (s, node) => stmt2BB ii node s) entry body
    in
        Cfg.link res exit;
        fill res (genJump exit);
        (id, cfg)
    end


fun ast2Iloc st (PROGRAM {funcs=fs, types=ts, ...}) =
    (app (addType types) ts; app (calcOffsets offsets) ts; map (func2Cfg st) fs)

end
