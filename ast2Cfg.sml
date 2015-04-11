signature AST2CFG = sig
    val ast2Cfg : program -> (string, cfg) HashTable.hash_table;
end

structure Ast2Cfg : AST2CFG = struct

fun mkHt () = HashTable.mkTable (HashString.hashString, op =)
                                (10, Fail "Not Found");

val funcs : (string, cfg) HashTable.hash_table = mkHt ();
val globals : (string, miniType) HashTable.hash_table = mkHt ();
val nextLabel = ref 0;

fun mkBB () =
    let
        val label = "L" ^ (Int.toString (!nextLabel));
    in
        (nextLabel := 1 + (!nextLabel);
         BB {prev=ref [], next=ref [], body=ref [], label=label})
    end


(*mmmm mutation... delicious*)
fun linkBB bb1 bb2 =
    let
        val BB {next=next, ...} = bb1;
        val BB {prev=prev, ...} = bb2;
    in
        next := bb2::(!next);
        prev := bb1::(!prev)
    end


fun expr2Ins L (EXP_NUM {value=value, ...}) =
    L
  | expr2Ins L (EXP_ID {id=id, ...}) =
    L
  | expr2Ins L (EXP_TRUE {...}) =
    L
  | expr2Ins L (EXP_FALSE {...}) =
    L
  | expr2Ins L EXP_UNDEFINED =
    L
  | expr2Ins L (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    L
  | expr2Ins L (EXP_UNARY {opr=opr, opnd=opnd, ...}) =
    L
  | expr2Ins L (EXP_DOT {lft=lft, prop=prop, ...}) =
    L
  | expr2Ins L (EXP_NEW {id=id, ...}) =
    L
  | expr2Ins L (EXP_INVOCATION {id=id, args=args, ...}) =
    L


fun fillBB exp (BB {body=body, ...})=
    let
        val instructions = List.rev (expr2Ins [] exp);
    in
        body := (!body) @ instructions
    end


(* CONVENTION: elseBlk is added first, then the thenBlk. So the thenBlk
 * will always be BEFORE the elseBlk in the list *)
fun stmt2BB f regs bb (ST_BLOCK stmts) =
    foldl (fn (s, bb) => stmt2BB f regs bb s) bb stmts
  | stmt2BB _ regs bb (ST_ASSIGN {target=target, source=source, ...}) =
    bb
  | stmt2BB _ regs bb (ST_PRINT {body=body, ...}) =
    bb
  | stmt2BB _ regs bb (ST_READ {id=id, ...}) =
    bb
  | stmt2BB f regs bb (ST_IF {guard=guard, thenBlk=thenBlk,
                              elseBlk=elseBlk, ...}) =
    let
        val exitBB = mkBB ();
        val thenBB = mkBB ();
        val elseBB = mkBB ();
        val thenResBB = stmt2BB f regs thenBB thenBlk;
        val elseResBB = stmt2BB f regs elseBB elseBlk;
    in
        linkBB bb elseBB;
        linkBB bb thenBB;
        linkBB elseResBB exitBB;
        linkBB thenResBB exitBB;
        exitBB
    end
  | stmt2BB f regs bb (ST_WHILE {guard=guard, body=body, ...}) =
    let
        val guardBB = mkBB ();
        val bodyBB = mkBB ();
        val exitBB = mkBB ();
        val bodyResBB = stmt2BB f regs bodyBB body;
    in
        linkBB bb guardBB;
        linkBB guardBB bodyBB;
        linkBB guardBB exitBB;
        linkBB bodyResBB guardBB;
        exitBB
    end
  | stmt2BB _ regs bb (ST_DELETE {exp=exp, ...}) =
    bb
  | stmt2BB f regs bb (ST_RETURN {exp=exp, ...}) =
    let
        val (CFG {exit=funcExitBB, ...}) = HashTable.lookup funcs f;
        val newBB = mkBB ();
    in
        (linkBB bb funcExitBB; newBB)
    end
  | stmt2BB _ regs bb (ST_INVOCATION {id=id, args=args, ...}) =
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
        val _ = HashTable.insert funcs (id, mkCfg entryBB exitBB f);
        val regs = assignRegs (HashTable.lookup funcs id);
        val resBB = foldl (fn (s, bb) => stmt2BB id regs bb s) bodyBB body;
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
