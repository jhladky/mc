signature STATIC = sig
    val staticCheck : string -> SymbolTable.symbolTable -> Ast.program -> unit
    val getExprType : string -> SymbolTable.symbolTable ->
                      Ast.expression -> Ast.typ
    val getLvalueType : string -> SymbolTable.symbolTable ->
                        Ast.lvalue -> Ast.typ
end

structure Static :> STATIC = struct
open HashTable
open Ast
open SymbolTable


exception Undefined        of int * string
exception BinOp            of int * binaryOperator * typ
exception UnOp             of int * unaryOperator * typ
exception TypeMismatch     of int * typ * typ
exception NotAFunction     of int * typ
exception ReturnMissing    of int
exception Invocation       of int (*I'll add arguments to this later.*)
exception NoMain           of int


(* null can be assigned to any struct type*)
fun checkType l (MT_INT, MT_INT) = ()
  | checkType l (MT_BOOL, MT_BOOL) = ()
  | checkType l (MT_VOID, MT_VOID) = ()
  | checkType l (MT_STRUCT _, MT_VOID) = ()
  | checkType l (MT_VOID, t2) = raise TypeMismatch (l, MT_VOID, t2)
  | checkType l (t1 as MT_STRUCT s1, t2 as MT_STRUCT s2) =
    if s1 = s2 then () else raise TypeMismatch (l, t1, t2)
  | checkType l (t1, t2) = raise TypeMismatch (l, t1, t2)


fun checkLvalue (STL {locals=locals, ...}) (LV_ID {id=id, line=l}) =
    (case find locals id of SOME t => t | NONE => raise Undefined (l, id))
  | checkLvalue stl (LV_DOT {lft=lft, prop=prop, line=l}) =
    let
        val STL {types=types, ...} = stl
        val r = case checkLvalue stl lft of
                    MT_STRUCT r => r
                  | t => raise TypeMismatch (l, MT_STRUCT "_", t)
    in
        case find (lookup types r) prop of
            SOME t => t
          | NONE => raise Undefined (l, prop)
    end


fun checkArgs l stl [] [] = ()
  | checkArgs l stl (x::xs) [] = raise Invocation l
  | checkArgs l stl [] (x::xs) = raise Invocation l
  | checkArgs l stl (VAR_DECL {typ=tParam, ...}::params) (arg::args)  =
    (checkType l (tParam, checkExpr stl arg); checkArgs l stl params args)


(*the type of the invocation expression is the RETURN TYPE of the function...*)
and checkInvocation l id args (stl as STL {globals=globals, funcs=funcs, ...}) =
    let
        val () = (case find globals id of
                      SOME MT_FUNC => ()
                    | SOME t => raise NotAFunction (l, t)
                    | NONE => raise NotAFunction (l, MT_VOID))
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
          | (BOP_PLUS, _) => raise BinOp (l, opr, MT_INT)
          | (BOP_MINUS, _) => raise BinOp (l, opr, MT_INT)
          | (BOP_TIMES, _) => raise BinOp (l, opr, MT_INT)
          | (BOP_DIVIDE, _) => raise BinOp (l, opr, MT_INT)
          | (BOP_LT, MT_INT) => MT_BOOL
          | (BOP_GT, MT_INT) => MT_BOOL
          | (BOP_LE, MT_INT) => MT_BOOL
          | (BOP_GE, MT_INT) => MT_BOOL
          | (BOP_LT, _) => raise BinOp (l, opr, MT_INT)
          | (BOP_GT, _) => raise BinOp (l, opr, MT_INT)
          | (BOP_LE, _) => raise BinOp (l, opr, MT_INT)
          | (BOP_GE, _) => raise BinOp (l, opr, MT_INT)
          | (BOP_AND, MT_BOOL) => MT_BOOL
          | (BOP_OR, MT_BOOL) => MT_BOOL
          | (BOP_AND, _) => raise BinOp (l, opr, MT_BOOL)
          | (BOP_OR, _) => raise BinOp (l, opr, MT_BOOL)
          | (BOP_EQ, _) => MT_BOOL
          | (BOP_NE, _) => MT_BOOL
    end


and checkExpr stl (EXP_NUM {value=n, ...}) = MT_INT
  | checkExpr (STL {locals=locals, ...}) (EXP_ID {id=id, line=l}) =
    (case find locals id of
        SOME t => t
      | NONE => raise Undefined (l, id))
  | checkExpr stl (EXP_TRUE {...}) = MT_BOOL
  | checkExpr stl (EXP_FALSE {...}) = MT_BOOL
  | checkExpr stl EXP_NULL = MT_VOID (*Not sure about this...*)
  | checkExpr stl (EXP_BINARY {opr=opr, lft=lft, rht=rht, line=l}) =
    checkBinExpr stl opr lft rht l
  | checkExpr stl (EXP_UNARY {opr=opr, opnd=opnd, line=l}) =
    (case (opr, checkExpr stl opnd) of
         (UOP_NOT, MT_BOOL) => MT_BOOL
       | (UOP_MINUS, MT_INT) => MT_INT
       | (UOP_NOT, _) => raise UnOp (l, opr, MT_BOOL)
       | (UOP_MINUS, _) => raise UnOp (l, opr, MT_INT))
  | checkExpr stl (EXP_DOT {lft=lft, prop=prop, line=l}) =
    let
        val r = case checkExpr stl lft of
                    MT_STRUCT r => r
                  | t => raise TypeMismatch (l, MT_STRUCT "_", t)
        val STL {types=types, ...} = stl
    in
        case find (lookup types r) prop of
            SOME t => t
          | NONE => raise Undefined (l, prop)
    end
  | checkExpr (STL {types=types, ...}) (EXP_NEW {id=id, line=l}) =
    (case find types id of
         SOME _ => MT_STRUCT id
       | NONE => raise TypeMismatch (l, MT_STRUCT "_", MT_VOID))
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
       | t => raise TypeMismatch (l, MT_INT, t))
  | checkStmt stl (ST_READ {id=id, line=l}) =
    (case checkLvalue stl id of
         MT_INT => false
       | t => raise TypeMismatch (l, MT_INT, t))
  | checkStmt stl (ST_IF {guard=guard, thenBlk=tBlk, elseBlk=eBlk, line=l}) =
    (case checkExpr stl guard of
         MT_BOOL => checkStmt stl tBlk andalso checkStmt stl eBlk
       | t => raise TypeMismatch (l, MT_BOOL, t))
  | checkStmt stl (ST_WHILE {guard=guard, body=body, line=l}) =
    (checkStmt stl body;
     case checkExpr stl guard of
         MT_BOOL => false
       | t => raise TypeMismatch (l, MT_BOOL, t))
  | checkStmt stl (ST_DELETE {exp=exp, line=l}) =
    (case checkExpr stl exp of
         MT_STRUCT s => false
       | t => raise TypeMismatch (l, MT_STRUCT "_", t))
  | checkStmt (stl as STL {returnType=rt, ...}) (ST_RETURN {exp=exp, line=l}) =
    (case exp of
         SOME e => checkType l (rt, checkExpr stl e)
       | NONE => checkType l (rt, MT_VOID);
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
        if doesRet = false andalso rt <> MT_VOID then raise ReturnMissing l
        else ()
    end


fun checkForMain (ST {funcs=funcs, ...}) =
    case find funcs "main" of
        SOME (FUNCTION {params=params, returnType=rt, ...}) =>
        if length params = 0 andalso rt = MT_INT then ()
        else raise NoMain 1
      | NONE => raise NoMain 1


(*Functions exposed in the signature are below.*)
fun getExprType id st exp = checkExpr (st2Stl id MT_VOID st) exp


fun getLvalueType id st lv = checkLvalue (st2Stl id MT_VOID st) lv


fun staticCheck file st (prog as PROGRAM {funcs=funcs, ...}) =
    let
        val fail = Util.fail file
    in
        (checkForMain st; List.app (checkFunc st) funcs)
        handle BinOp (ln, opr, t) =>
               fail ln ("Operator " ^ (binOpToStr opr) ^ " requires an " ^
                        (typeToStr t) ^ " type.\n")
             | UnOp (ln, opr, t) =>
               fail ln ("Operator " ^ (unOpToStr opr) ^ " requires an " ^
                        (typeToStr t) ^ " type.\n")
             | TypeMismatch (ln, t1, t2) =>
               fail ln ("Type " ^ (typeToStr t1) ^ "required (supplied "
                        ^ (typeToStr t2) ^ ").\n")
             | NotAFunction (ln, t) =>
               fail ln ("Type " ^ (typeToStr t) ^ " is not callable.\n")
             | Undefined (ln, id) =>
               fail ln ("Undefined variable " ^ id ^ ".\n")
             | Invocation ln => fail ln "Bad function invocation.\n"
             | ReturnMissing ln =>
               fail ln "Function does not return on all paths.\n"
             | NoMain ln => fail ln "No function matching `main` signature.\n"
    end

end
