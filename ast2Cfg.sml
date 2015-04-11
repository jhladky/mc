signature AST2CFG = sig
    val ast2Cfg : program ->
                  (string, basicBlock * basicBlock) HashTable.hash_table;
end

structure Ast2Cfg : AST2CFG = struct

fun mkHt () = HashTable.mkTable (HashString.hashString, op =)
                                (10, Fail "Not Found");

(*The pair is (entryBlock, exitBlock)*)
val funcs : (string, basicBlock * basicBlock) HashTable.hash_table = mkHt ();
val nextLabel = ref 0;

datatype direction = PREV | NEXT;

fun mkBB prev next body =
    let
        val label = "L" ^ (Int.toString (!nextLabel));
    in
        (nextLabel := 1 + (!nextLabel);
         BB {prev=ref prev, next=ref next, body=ref body, label=label})
    end

(*mmmm mutation... delicious*)
fun prependBB bb dir source =
    case (bb, dir) of
        (BB {prev=prev, ...}, PREV) => prev := source::(!prev)
      | (BB {next=next, ...}, NEXT) => next := source::(!next)
;

fun expr2BB bb (EXP_NUM {value=value, ...}) =
    bb
  | expr2BB bb (EXP_ID {id=id, ...}) =
    bb
  | expr2BB bb (EXP_TRUE {...}) =
    bb
  | expr2BB bb (EXP_FALSE {...}) =
    bb
  | expr2BB bb EXP_UNDEFINED =
    bb
  | expr2BB bb (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    bb
  | expr2BB bb (EXP_UNARY {opr=opr, opnd=opnd, ...}) =
    bb
  | expr2BB bb (EXP_DOT {lft=lft, prop=prop, ...}) =
    bb
  | expr2BB bb (EXP_NEW {id=id, ...}) =
    bb
  | expr2BB bb (EXP_INVOCATION {id=id, args=args, ...}) =
    bb
;

fun stmt2BB f bb (ST_BLOCK stmts) =
    foldl (fn (s, bb) => stmt2BB f bb s) bb stmts
  | stmt2BB f bb (ST_ASSIGN {target=target, source=source, ...}) =
    bb (*assign can't change the control flow*)
  | stmt2BB f bb (ST_PRINT {body=body, ...}) =
    bb (*print can't change the control flow*)
  | stmt2BB f bb (ST_READ {id=id, ...}) =
    bb (*read can't change the control flow*)
  | stmt2BB f bb (ST_IF {guard=guard, thenBlk=thenBlk, elseBlk=elseBlk, ...}) =
    (*We need to be VERY careful about the order in which we add the thenBlk
     * and the elseBlk to the exitBB prev list.
     * CONVENTION: elseBlk is added first, then the thenBlk. So the thenBlk
     * will always be BEFORE the elseBlk in the list *)
    let
        val exitBB = mkBB [] [] [];
        val thenBB = mkBB [bb] [] [];
        val elseBB = mkBB [bb] [] [];
        val thenResBB = stmt2BB f thenBB thenBlk;
        val elseResBB = stmt2BB f elseBB elseBlk;
    in
        (prependBB bb NEXT elseBB;
         prependBB bb NEXT thenBB;
         prependBB elseResBB NEXT exitBB;
         prependBB thenResBB NEXT exitBB;
         prependBB exitBB PREV elseResBB;
         prependBB exitBB PREV thenResBB;
         exitBB)
    end
  | stmt2BB f bb (ST_WHILE {guard=guard, body=body, ...}) =
    let
        val guardBB = mkBB [bb] [] [];
        val bodyBB = mkBB [guardBB] [] [];
        val bodyResBB = stmt2BB f bodyBB body;
        val exitBB = mkBB [guardBB] [] [];
    in
        (prependBB bb NEXT guardBB;
         prependBB bodyResBB NEXT guardBB;
         prependBB guardBB PREV bodyResBB;
         prependBB guardBB NEXT bodyBB;
         prependBB guardBB NEXT exitBB;
         exitBB)
    end
  | stmt2BB f bb (ST_DELETE {exp=exp, ...}) =
    bb (*delete can't change the control flow*)
  | stmt2BB f bb (ST_RETURN {exp=exp, ...}) =
    let
        val (_, funcExitBB) = HashTable.lookup funcs f;
        val newBB = mkBB [] [] [];
    in
        (prependBB bb NEXT funcExitBB;
         prependBB funcExitBB PREV bb;
         newBB)
    end
  | stmt2BB f bb (ST_INVOCATION {id=id, args=args, ...}) =
    bb (*Invocation does not change the control flow*)
;

fun func2Cfg (FUNCTION {id=id, params=params, decls=decls, body=body, ...}) =
    let
        val entryBB = mkBB [] [] [];
        val exitBB = mkBB [] [] [];
        val bodyBB = mkBB [entryBB] [] [];
        val _ = HashTable.insert funcs (id, (entryBB, exitBB));
        val resBB = foldl (fn (s, bb) => stmt2BB id bb s) bodyBB body;
    in
        (prependBB entryBB NEXT bodyBB;
         prependBB resBB NEXT exitBB;
         prependBB exitBB PREV resBB)
    end
;

(*we're going to need to get the symbol table in here also*)
fun ast2Cfg (PROGRAM {funcs=fs, ...}) = (app func2Cfg fs; funcs);

end;
