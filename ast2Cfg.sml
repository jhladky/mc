signature AST2CFG = sig
    val ast2Cfg : program -> (string, cfg) HashTable.hash_table;
end

structure Ast2Cfg : AST2CFG = struct

fun mkHt () = HashTable.mkTable (HashString.hashString, op =)
                                (10, Fail "Not Found");

type funcEntry = {nextReg: int ref, regs: (string, int) HashTable.hash_table};

val globals : (string, miniType) HashTable.hash_table = mkHt ();
val funcs : (string, cfg) HashTable.hash_table = mkHt ();
val funcRegs : (string, funcEntry) HashTable.hash_table = mkHt ();
val nextLabel = ref 0;

fun mkBB () =
    let
        val label = "L" ^ (Int.toString (!nextLabel));
    in
        nextLabel := 1 + (!nextLabel);
        BB {prev=ref [], next=ref [], body=ref [], label=label}
    end


(*mmmm mutation... delicious*)
fun linkBB bb1 bb2 =
    let
        val (BB {next=next, ...}) = bb1;
        val (BB {prev=prev, ...}) = bb2;
    in
        next := bb2::(!next);
        prev := bb1::(!prev)
    end


(*this is definitely going to need some work*)
fun getReg regs bb id =
    case HashTable.find regs id of
        SOME dest => dest
      | NONE => raise Fail "global"


fun getNextReg f =
    let
        val {regs=_, nextReg=nextReg} = HashTable.lookup funcRegs f;
    in
        !nextReg before nextReg := 1 + (!nextReg)
    end


fun binExpr2Ins f L opr lft rht =
    let
        val (dest1, L1) = expr2Ins f L lft;
        val (dest2, L2) = expr2Ins f L1 rht;
        val newDest = getNextReg f;
    in
        case opr of
            BOP_PLUS =>
            (newDest, (INS_RRR {opcode=OP_ADD, r1=dest1, r2=dest2,
                                dest=newDest})::L2)
          | BOP_MINUS =>
            (newDest, (INS_RRR {opcode=OP_SUB, r1=dest1, r2=dest2,
                                dest=newDest})::L2)
          | BOP_TIMES =>
            (newDest, (INS_RRR {opcode=OP_MULT, r1=dest1, r2=dest2,
                                dest=newDest})::L2)
          | BOP_DIVIDE =>
            (newDest, (INS_RRR {opcode=OP_DIV, r1=dest1, r2=dest2,
                                dest=newDest})::L2)
          | BOP_EQ =>
            (1, L2)
          | BOP_NE =>
            (1, L2)
          | BOP_LT =>
            (1, L2)
          | BOP_GT =>
            (1, L2)
          | BOP_LE =>
            (1, L2)
          | BOP_GE =>
            (1, L2)
          | BOP_AND =>
            (newDest, (INS_RRR {opcode=OP_AND, r1=dest1, r2=dest2,
                                dest=newDest})::L2)
          | BOP_OR =>
            (newDest, (INS_RRR {opcode=OP_OR, r1=dest1, r2=dest2,
                                dest=newDest})::L2)
    end


and unExpr2Ins f L opr opnd =
    let
        val (dest1, L1) = expr2Ins f L opnd;
        val newDest = getNextReg f;
    in
        case opr of
            UOP_NOT =>
            (newDest, (INS_RIR {opcode=OP_XORI, immed=0xFFFF, r1=dest1, dest=newDest})::
                       L1)
          | UOP_MINUS =>
            let
                val newDest1 = getNextReg f;
            in
                (newDest1, (INS_RRR {opcode=OP_SUB, r1=newDest, r2=dest1, dest=newDest1})::
                           (INS_IR {opcode=OP_LOADI, immed=0, r1=newDest})::
                           L1)
            end
    end

and expr2Ins f L (EXP_NUM {value=value, ...}) =
    let
        val dest = getNextReg f;
    in
        (dest, (INS_IR {opcode=OP_LOADI, immed=value, r1=dest})::L)
    end
  | expr2Ins f L (EXP_ID {id=id, ...}) =
    (1, L)
  | expr2Ins f L (EXP_TRUE {...}) =
    let
        val dest = getNextReg f;
    in
        (dest, (INS_IR {opcode=OP_LOADI, immed=1, r1=dest})::L)
    end
  | expr2Ins f L (EXP_FALSE {...}) =
    let
        val dest = getNextReg f;
    in
        (dest, (INS_IR {opcode=OP_LOADI, immed=0, r1=dest})::L)
    end
  | expr2Ins f L EXP_UNDEFINED =
    (1, L)
  | expr2Ins f L (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    binExpr2Ins f L opr lft rht
  | expr2Ins f L (EXP_UNARY {opr=opr, opnd=opnd, ...}) =
    unExpr2Ins f L opr opnd
  | expr2Ins f L (EXP_DOT {lft=lft, prop=prop, ...}) =
    (1, L)
  | expr2Ins f L (EXP_NEW {id=id, ...}) =
    (1, L)
  | expr2Ins f L (EXP_INVOCATION {id=id, args=args, ...}) =
    (1, L)

(*so both fillBB and expr2Ins need to pass UP the
 * DESTINATION register*)

fun fillBB bb f exp =
    let
        val (dest, instructions) = expr2Ins f [] exp;
        val (BB {body=body, ...}) = bb;
    in
        body := (!body) @ (List.rev instructions);
        dest
    end


fun addInsBB bb ins =
    let
        val (BB {body=body, ...}) = bb;
    in
        body := (!body) @ [ins]
    end


(* CONVENTION: elseBlk is added first, then the thenBlk. So the thenBlk
 * will always be BEFORE the elseBlk in the list *)
fun stmt2BB f bb (ST_BLOCK stmts) =
    foldl (fn (s, bb) => stmt2BB f bb s) bb stmts
  | stmt2BB _ bb (ST_ASSIGN {target=target, source=source, ...}) =
    bb
  | stmt2BB f bb (ST_PRINT {body=body, endl=endl, ...}) =
    let
        val dest = fillBB bb f body;
        val opcode = if endl then OP_PRINTLN else OP_PRINT;
    in
        addInsBB bb (INS_R {opcode=opcode, r1=dest});
        bb
    end
  | stmt2BB _ bb (ST_READ {id=id, ...}) =
    bb
  | stmt2BB f bb (ST_IF {guard=guard, thenBlk=thenBlk,
                              elseBlk=elseBlk, ...}) =
    let
        val exitBB = mkBB ();
        val thenBB = mkBB ();
        val elseBB = mkBB ();
        val thenResBB = stmt2BB f thenBB thenBlk;
        val elseResBB = stmt2BB f elseBB elseBlk;
    in
        fillBB bb f guard;
        linkBB bb elseBB;
        linkBB bb thenBB;
        linkBB elseResBB exitBB;
        linkBB thenResBB exitBB;
        exitBB
    end
  | stmt2BB f bb (ST_WHILE {guard=guard, body=body, ...}) =
    let
        val guardBB = mkBB ();
        val bodyBB = mkBB ();
        val exitBB = mkBB ();
        val bodyResBB = stmt2BB f bodyBB body;
    in
        fillBB guardBB f guard;
        linkBB bb guardBB;
        linkBB guardBB bodyBB;
        linkBB guardBB exitBB;
        linkBB bodyResBB guardBB;
        exitBB
    end
  | stmt2BB _ bb (ST_DELETE {exp=exp, ...}) =
    bb
  | stmt2BB f bb (ST_RETURN {exp=exp, ...}) =
    let
        val (CFG {exit=funcExitBB, ...}) = HashTable.lookup funcs f;
        val newBB = mkBB ();
    in
        (linkBB bb funcExitBB; newBB)
    end
  | stmt2BB _ bb (ST_INVOCATION {id=id, args=args, ...}) =
    bb


fun mkCfg entry exit (FUNCTION {params=params, decls=decls, ...}) =
    let
        val ht = mkHt ();
        val addVD = (fn (VAR_DECL {id=s, typ=t, ...}) =>
                        HashTable.insert ht (s, t));
    in
        app addVD decls;
        app addVD params;
        CFG {locals=ht, entry=entry, exit=exit}
    end


fun assignRegs (CFG {locals=locals, ...}) =
    let
        val n = ref 0;
    in
        HashTable.map (fn _ => !n before n := 1 + (!n)) locals
    end


(*When we enter a function we have to assign registers to all
* the local variables and parameters*)
fun func2Cfg (f as FUNCTION {id=id, body=body, ...}) =
    let
        val entryBB = mkBB ();
        val exitBB = mkBB ();
        val bodyBB = mkBB ();
        val regs = assignRegs (HashTable.lookup funcs id);
        val funcEntry = {regs=regs, nextReg=ref (HashTable.numItems regs)};
        val _ = HashTable.insert funcs (id, mkCfg entryBB exitBB f);
        val _ = HashTable.insert funcRegs (id, funcEntry);
        val resBB = foldl (fn (s, bb) => stmt2BB id bb s) bodyBB body;
    in
        linkBB entryBB bodyBB;
        linkBB resBB exitBB
    end


fun ast2Cfg (PROGRAM {funcs=fs, decls=decls, ...}) =
    (app (fn VAR_DECL {id=s, typ=t, ...} =>
             HashTable.insert globals (s, t)) decls;
     app func2Cfg fs;
     funcs)

end
