signature STATIC = sig
    val staticCheck : string -> SymbolTable.symbolTable -> Ast.program -> unit
    val getExprType : string -> SymbolTable.symbolTable ->
                      Ast.expression -> Ast.typ
end

structure Static :> STATIC = struct
open HashTable
open Ast
open SymbolTable

exception UndefException        of int * string
exception BinOpException        of int * binaryOperator * typ
exception UnOpException         of int * unaryOperator * typ
exception TypeMatchException    of int * typ * typ
exception NotAFunctionException of int * typ
exception NotAStructException   of int * typ
exception PRException           of int * typ
exception BooleanGuardException of int * typ
exception NoReturnException     of int
exception BadReturnException    of int
exception InvocationException   of int (*I'll add arguments to this later.*)
exception NoMainException       of int


(* null can be assigned to any struct type*)
fun checkType l (MT_INT, MT_INT) = ()
  | checkType l (MT_BOOL, MT_BOOL) = ()
  | checkType l (MT_VOID, MT_VOID) = () (*This is not good...*)
  | checkType l (MT_STRUCT _, MT_VOID) = ()
  | checkType l (MT_VOID, t2) = raise TypeMatchException (l, MT_VOID, t2)
  | checkType l (t1 as MT_STRUCT s1, t2 as MT_STRUCT s2) =
    if s1 = s2 then () else raise TypeMatchException (l, t1, t2)
  | checkType l (t1, t2) = raise TypeMatchException (l, t1, t2)


fun checkLvalue (STL {locals=locals, ...}) (LV_ID {id=id, line=l}) =
    (case find locals id of SOME t => t | NONE => raise UndefException (l, id))
  | checkLvalue stl (LV_DOT {lft=lft, prop=prop, line=l}) =
    let
        val STL {types=types, ...} = stl
        val r = case checkLvalue stl lft of
                    MT_STRUCT r => r
                  | t => raise NotAStructException (l, t)
    in
        case find (lookup types r) prop of
            SOME t => t
          | NONE => raise UndefException (l, prop)
    end


fun checkArgs l stl [] [] = ()
  | checkArgs l stl (x::xs) [] = raise InvocationException l
  | checkArgs l stl [] (x::xs) = raise InvocationException l
  | checkArgs l stl (VAR_DECL {typ=tParam, ...}::params) (arg::args)  =
    (checkType l (tParam, checkExpr stl arg); checkArgs l stl params args)


(*the type of the invocation expression is the RETURN TYPE of the function...*)
and checkInvocation l id args (stl as STL {globals=globals, funcs=funcs, ...}) =
    let
        val () = (case find globals id of
                      SOME MT_FUNC => ()
                    | SOME t => raise NotAFunctionException (l, t)
                    | NONE => raise NotAFunctionException (l, MT_VOID))
        (* We can do this safely since we already checked to
         * make sure f was a function*)
        val (FUNCTION {params=params, returnType=rt, ...}) = lookup funcs id
    in
        checkArgs l stl params args; rt
    end


and checkBinExpr stl opr lft rht l =
    let
        val tLft = checkExpr stl lft;
        val tRht = checkExpr stl rht;
    in
        checkType l (tLft, tRht);
        case (opr, tLft) of
            (BOP_PLUS, MT_INT) => MT_INT
          | (BOP_MINUS, MT_INT) => MT_INT
          | (BOP_TIMES, MT_INT) => MT_INT
          | (BOP_DIVIDE, MT_INT) => MT_INT
          | (BOP_PLUS, _) => raise BinOpException (l, opr, MT_INT)
          | (BOP_MINUS, _) => raise BinOpException (l, opr, MT_INT)
          | (BOP_TIMES, _) => raise BinOpException (l, opr, MT_INT)
          | (BOP_DIVIDE, _) => raise BinOpException (l, opr, MT_INT)
          | (BOP_LT, MT_INT) => MT_BOOL
          | (BOP_GT, MT_INT) => MT_BOOL
          | (BOP_LE, MT_INT) => MT_BOOL
          | (BOP_GE, MT_INT) => MT_BOOL
          | (BOP_LT, _) => raise BinOpException (l, opr, MT_INT)
          | (BOP_GT, _) => raise BinOpException (l, opr, MT_INT)
          | (BOP_LE, _) => raise BinOpException (l, opr, MT_INT)
          | (BOP_GE, _) => raise BinOpException (l, opr, MT_INT)
          | (BOP_AND, MT_BOOL) => MT_BOOL
          | (BOP_OR, MT_BOOL) => MT_BOOL
          | (BOP_AND, _) => raise BinOpException (l, opr, MT_BOOL)
          | (BOP_OR, _) => raise BinOpException (l, opr, MT_BOOL)
          | (BOP_EQ, _) => MT_BOOL
          | (BOP_NE, _) => MT_BOOL
    end


and checkExpr stl (EXP_NUM {value=n, ...}) = MT_INT
  | checkExpr (STL {locals=locals, ...}) (EXP_ID {id=id, line=l}) =
    (case find locals id of
        SOME t => t
      | NONE => raise UndefException (l, id))
  | checkExpr stl (EXP_TRUE {...}) = MT_BOOL
  | checkExpr stl (EXP_FALSE {...}) = MT_BOOL
  | checkExpr stl EXP_NULL = MT_VOID (*Not sure about this...*)
  | checkExpr stl (EXP_BINARY {opr=opr, lft=lft, rht=rht, line=l}) =
    checkBinExpr stl opr lft rht l
  | checkExpr stl (EXP_UNARY {opr=opr, opnd=opnd, line=l}) =
    (case (opr, checkExpr stl opnd) of
         (UOP_NOT, MT_BOOL) => MT_BOOL
       | (UOP_MINUS, MT_INT) => MT_INT
       | (UOP_NOT, _) => raise UnOpException (l, opr, MT_BOOL)
       | (UOP_MINUS, _) => raise UnOpException (l, opr, MT_INT))
  | checkExpr stl (EXP_DOT {lft=lft, prop=prop, line=l}) =
    let
        val r = case checkExpr stl lft of
                    MT_STRUCT r => r
                  | t => raise NotAStructException (l, t)
        val STL {types=types, ...} = stl
    in
        case find (lookup types r) prop of
            SOME t => t
          | NONE => raise UndefException (l, prop)
    end
  | checkExpr (STL {types=types, ...}) (EXP_NEW {id=id, line=l}) =
    (case find types id of
         SOME _ => MT_STRUCT id
       | NONE => raise NotAStructException (l, MT_VOID))
  | checkExpr stl (EXP_INVOCATION {id=id, args=args, line=l}) =
    checkInvocation l id args stl


(*Returns a boolean indicating whether the statement returns on all paths*)
fun checkStmt stl (ST_BLOCK stmts) =
    checkStmts stl stmts
  | checkStmt stl (ST_ASSIGN {target=target, source=source, line=l}) =
    (checkType l (checkLvalue stl target, checkExpr stl source); false)
  | checkStmt stl (ST_PRINT {body=body, line=l, ...}) =
    (case checkExpr stl body of
         MT_INT => false
       | t => raise PRException (l, t))
  | checkStmt stl (ST_READ {id=id, line=l}) =
    (case checkLvalue stl id of
         MT_INT => false
       | t => raise PRException (l, t))
  | checkStmt stl (ST_IF {guard=guard, thenBlk=tBlk, elseBlk=eBlk, line=l}) =
    (case checkExpr stl guard of
         MT_BOOL => checkStmt stl tBlk andalso checkStmt stl eBlk
       | t => raise BooleanGuardException (l, t))
  | checkStmt stl (ST_WHILE {guard=guard, body=body, line=l}) =
    (checkStmt stl body;
     case checkExpr stl guard of
         MT_BOOL => false
       | t => raise BooleanGuardException (l, t))
  | checkStmt stl (ST_DELETE {exp=exp, line=l}) =
    (case checkExpr stl exp of
         MT_STRUCT s => false
       | t => raise NotAStructException (l, t))
  | checkStmt (stl as STL {returnType=rt, ...}) (ST_RETURN {exp=exp, line=l}) =
    (case exp of
         SOME e => checkType l (rt, checkExpr stl e)
       | NONE => checkType l (rt, checkExpr stl EXP_NULL);
     true)
  | checkStmt stl (ST_INVOCATION {id=id, args=args, line=l}) =
    (checkInvocation l id args stl; false)


and checkStmts1 retDetect stl [] = retDetect
  | checkStmts1 retDetect stl (stmt::stmts) =
    if checkStmt stl stmt then checkStmts1 true stl stmts
    else checkStmts1 retDetect stl stmts


and checkStmts stl L = checkStmts1 false stl L


(*move to SymbolTable struct later*)
fun st2Stl id rt (ST {types=ts, globals=gs, funcs=fs, locals=ls}) =
    STL {types=ts, globals=gs, locals=lookup ls id, funcs=fs, returnType=rt}


fun checkFunc st (FUNCTION {returnType=rt, body=body, line=l, id=id, ...}) =
    let
        val doesRet = checkStmts (st2Stl id rt st) body
    in
        if doesRet = false andalso rt <> MT_VOID then raise NoReturnException l
        else ()
    end


fun checkForMain (ST {funcs=funcs, ...}) =
    case find funcs "main" of
        SOME (FUNCTION {params=params, returnType=rt, ...}) =>
        if length params = 0 andalso rt = MT_INT then ()
        else raise NoMainException 1
      | NONE => raise NoMainException 1


(*The two functions exposed in the signature are below*)
fun getExprType id st exp = checkExpr (st2Stl id MT_VOID st) exp


fun staticCheck file st (prog as PROGRAM {funcs=funcs, ...}) =
    let
        val fail = Util.fail file
    in
        (checkForMain st; List.app (checkFunc st) funcs)
        handle BinOpException (ln, opr, t) =>
               fail ln ("Operator " ^ (binOpToStr opr) ^ " requires an " ^
                        (typeToStr t) ^ " type.\n")
             | UnOpException (ln, opr, t) =>
               fail ln ("Operator " ^ (unOpToStr opr) ^ " requires an " ^
                        (typeToStr t) ^ " type.\n")
             | TypeMatchException (ln, t1, t2) =>
               fail ln ("Types " ^ (typeToStr t1) ^ " and " ^ (typeToStr t2) ^
                        " do not match.\n")
             | NotAFunctionException (ln, t) =>
               fail ln ("Type " ^ (typeToStr t) ^ " is not callable.\n")
             | NotAStructException (ln, t) =>
               fail ln ("Expression requires a struct type (Supplied " ^
                        (typeToStr t) ^ ").\n")
             | PRException (ln, t) =>
               fail ln ("`print` and `read` require an integer argument " ^
                        "(Supplied " ^ (typeToStr t) ^ ").\n")
             | BooleanGuardException (ln, t) =>
               fail ln ("Statement requires a boolean " ^
                        "expression (Supplied " ^ (typeToStr t) ^ ").\n")
             | UndefException (ln, id) =>
               fail ln ("Undefined variable " ^ id ^ ".\n")
             | InvocationException ln => fail ln "Bad function invocation.\n"
             | NoReturnException ln =>
               fail ln "Function does not return on all paths.\n"
             | NoMainException ln =>
               fail ln "No function matching `main` signature.\n"
    end

end
