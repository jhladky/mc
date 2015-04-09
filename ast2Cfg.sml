(*So the main function is going to be our entry point for the program...
how to get this info to the function..
we could just get it again I suppose.*)

fun expr2Bb bb (EXP_NUM {value=value, ...}) =
    ()
  | expr2Bb bb (EXP_ID {id=id, ...}) =
    ()
  | expr2Bb bb (EXP_TRUE {...}) =
    ()
  | expr2Bb bb (EXP_FALSE {...}) =
    ()
  | expr2Bb bb EXP_UNDEFINED =
    ()
  | expr2Bb bb (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    ()
  | expr2Bb bb (EXP_UNARY {opr=opr, opnd=opnd, ...}) =
    ()
  | expr2Bb bb (EXP_DOT {lft=lft, prop=prop, ...}) =
    ()
  | expr2Bb bb (EXP_NEW {id=id, ...}) =
    ()
  | expr2Bb bb (EXP_INVOCATION {id=id, args=args, ...}) =
    ()
;

(*So the question we have to ask for each of these is, can it change the control flow?
 * If it can, then we need to set up the bb structure, otherwise just return it back up
 * (for now). When we generate the ILOC, we will fill the body with instructions*)
fun stmt2Bb bb (ST_BLOCK stmts) =
    bb
  | stmt2Bb bb (ST_ASSIGN {target=target, source=source, ...}) =
    bb
  | stmt2Bb bb (ST_PRINT {body=body, ...}) =
    bb
  | stmt2Bb bb (ST_READ {id=id, ...}) =
    bb
  | stmt2Bb bb (ST_IF {guard=guard, thenBlk=thenBlk, elseBlk=elseBlk, ...}) =
    bb
  | stmt2Bb bb (ST_WHILE {guard=guard, body=body, ...}) =
    bb
  | stmt2Bb bb (ST_DELETE {exp=exp, ...}) =
    bb
  | stmt2Bb bb (ST_RETURN {exp=exp, ...}) =
    bb
  | stmt2Bb bb (ST_INVOCATION {id=id, args=args, ...}) =
    bb
;

fun func2Bb bb (FUNCTION {id=id, params=params, decls=decls, body=body, ...}) =
    foldr (fn (s, bb) => stmt2Bb bb s) bb body
;

fun ast2Cfg (PROGRAM {types=types, decls=decls, funcs=funcs}) =
    let
        val entry = BB {prev=[], next=[], body=[]}
        val exit = foldr (fn (f, bb) => func2Bb bb f) entry funcs
    in
        CFG {entry=entry, exit=exit}
    end
;
