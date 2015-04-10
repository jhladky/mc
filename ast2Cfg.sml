fun makeHt () = HashTable.mkTable (HashString.hashString, op =)
                                  (10, Fail "Not Found");

(*The pair is (entryBlock, exitBlock)*)
val funcs : (string, basicBlock * basicBlock) HashTable.hash_table = makeHt ();

(*So the main function is going to be our entry point for the program...
how to get this info to the function..
we could just get it again I suppose.*)

fun expr2BB bb (EXP_NUM {value=value, ...}) =
    ()
  | expr2BB bb (EXP_ID {id=id, ...}) =
    ()
  | expr2BB bb (EXP_TRUE {...}) =
    ()
  | expr2BB bb (EXP_FALSE {...}) =
    ()
  | expr2BB bb EXP_UNDEFINED =
    ()
  | expr2BB bb (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    ()
  | expr2BB bb (EXP_UNARY {opr=opr, opnd=opnd, ...}) =
    ()
  | expr2BB bb (EXP_DOT {lft=lft, prop=prop, ...}) =
    ()
  | expr2BB bb (EXP_NEW {id=id, ...}) =
    ()
  | expr2BB bb (EXP_INVOCATION {id=id, args=args, ...}) =
    ()
;

datatype direction = PREV | NEXT;

fun mkBB prev next body = BB {prev=ref prev, next=ref next, body=ref body};

(*mmmm mutation... delicious*)
fun prependBB bb dir source =
    case (bb, dir) of
        (BB {prev=prev, ...}, PREV) => prev := source::(!prev)
      | (BB {next=next, ...}, NEXT) => next := source::(!next)
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
        val thenBB = stmt2BB f (mkBB [bb] [exitBB] []) thenBlk;
        val elseBB = stmt2BB f (mkBB [bb] [exitBB] []) elseBlk;
    in
        (prependBB exitBB PREV elseBB;
         prependBB exitBB PREV thenBB;
         prependBB bb NEXT elseBB;
         prependBB bb NEXT thenBB;
         exitBB)
    end
  | stmt2BB f bb (ST_WHILE {guard=guard, body=body, ...}) =
    let
        val guardBB = mkBB [bb] [] [];
        val bodyBB = stmt2BB f (mkBB [guardBB] [guardBB] []) body;
        val exitBB = mkBB [guardBB] [] [];
    in
        (prependBB bb NEXT guardBB;
         prependBB guardBB NEXT bodyBB;
         prependBB guardBB NEXT exitBB;
         prependBB guardBB PREV bodyBB;
         exitBB)
    end
  | stmt2BB f bb (ST_DELETE {exp=exp, ...}) =
    bb (*delete can't change the control flow*)
  | stmt2BB f bb (ST_RETURN {exp=exp, ...}) =
    let
        val (_, funcExitBB) = HashTable.lookup funcs f;
        val newBB = mkBB [bb] [] [];
    in
        (prependBB bb NEXT funcExitBB;
         prependBB bb NEXT newBB;
         prependBB funcExitBB PREV bb;
         newBB)
    end
  | stmt2BB f bb (ST_INVOCATION {id=id, args=args, ...}) =
    let
        val (entryBB, exitBB) = HashTable.lookup funcs id;
        val newBB = mkBB [exitBB] [] [];
    in
        (prependBB exitBB NEXT newBB;
         prependBB entryBB PREV bb;
         prependBB bb NEXT entryBB;
         newBB)
    end
;

fun func2Cfg (FUNCTION {id=id, params=params, decls=decls, body=body, ...}) =
    let
        val entryBB = mkBB [] [] [];
        val exitBB = mkBB [] [] [];
        val _ = HashTable.insert funcs (id, (entryBB, exitBB));
        val bodyBB = foldl (fn (s, bb) => stmt2BB id bb s) entryBB body;
    in
        if entryBB = bodyBB then (
            prependBB entryBB NEXT exitBB;
            prependBB exitBB PREV entryBB
        ) else (
            prependBB entryBB NEXT bodyBB;
            prependBB bodyBB NEXT exitBB;
            prependBB exitBB PREV bodyBB
        )
    end
;

fun ast2Cfg (PROGRAM {types=types, decls=decls, funcs=funcs}) =
    let
        (*Not sure what to do with these now...*)
        val entry = mkBB [] [] [];
        val exit = mkBB [] [] [];
        (*So this foldr is pretty much wrong now....*)
        (* val exit = foldr (fn (f, bb) => func2BB bb f) entry funcs *)
    in
        (app func2Cfg funcs;
         CFG {entry=entry, exit=exit})
    end
;
