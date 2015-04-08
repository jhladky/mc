(*This is going to stay global for now until I figure out what to do with it*)
fun makeHt () = HashTable.mkTable (HashString.hashString, op =)
                                  (10, Fail "Not Found");

structure staticCheck :
          sig
              val staticCheck : string -> program -> unit
          end
= struct

exception UndefException of int * string;
exception BinOpException of int * binaryOperator * miniType;
exception UnOpException of int * unaryOperator * miniType;
exception TypeMatchException of int * miniType * miniType;
exception NotAFunctionException of int * miniType;
exception NotAStructException of int * miniType;
exception PrintException of int * miniType;
exception BooleanGuardException of int * miniType;
exception NoReturnException of int;
exception BadReturnException of int;
exception InvocationException of int; (*I'll add arguments to this later.*)
exception NoMainException of int

(* Variables and functions are in the same namespace,
 * but structure names are in a DIFFERENT namespace.*)
val types : (string, (string, miniType) HashTable.hash_table)
                HashTable.hash_table = makeHt ();
val decls : (string, miniType) HashTable.hash_table = makeHt ();
val funcs : (string, function) HashTable.hash_table = makeHt ();

(*HELPER FUNCTIONS*)

fun fail file l msg =
    (TextIO.output (TextIO.stdErr, file ^ ":" ^ (Int.toString l) ^ ":" ^ msg);
     OS.Process.exit OS.Process.failure)
;

fun binOp2Str BOP_PLUS = "+"
  | binOp2Str BOP_MINUS = "-"
  | binOp2Str BOP_TIMES = "*"
  | binOp2Str BOP_DIVIDE = "/"
  | binOp2Str BOP_MOD = "%"
  | binOp2Str BOP_EQ = "=="
  | binOp2Str BOP_NE = "!="
  | binOp2Str BOP_LT = "<"
  | binOp2Str BOP_GT = ">"
  | binOp2Str BOP_LE = "<="
  | binOp2Str BOP_GE = ">="
  | binOp2Str BOP_AND = "&&"
  | binOp2Str BOP_OR = "||"
;

fun unOp2Str UOP_NOT = "!" | unOp2Str UOP_MINUS = "-";

fun typ2Str MT_INT = "integer"
  | typ2Str MT_VOID = "void"
  | typ2Str MT_BOOL = "boolean"
  | typ2Str MT_FUNC = "function"
  | typ2Str (MT_STRUCT s) = "struct " ^ s
;

(*AST FUNCTIONS*)
(* null can be assigned to any struct type*)
fun checkType l (MT_INT, MT_INT) = ()
  | checkType l (MT_BOOL, MT_BOOL) = ()
  | checkType l (MT_STRUCT _, MT_VOID) = ()
  | checkType l (MT_VOID, t2) = raise TypeMatchException (l, MT_VOID, t2)
  | checkType l (t1 as MT_STRUCT s1, t2 as MT_STRUCT s2) =
    if s1 = s2 then () else raise TypeMatchException (l, t1, t2)
  | checkType l (t1, t2) = raise TypeMatchException (l, t1, t2)
;

fun checkLvalue ht (LVALUE lval) =
    case HashTable.find ht lval of
        SOME t => t
      | NONE => raise UndefException (0, lval) (*Fix this later*)
;

fun checkArgs l ht [] [] = ()
  | checkArgs l ht (x::xs) [] = raise InvocationException l
  | checkArgs l ht [] (x::xs) = raise InvocationException l
  | checkArgs l ht (VAR_DECL {typ=tParam, ...}::params) (arg::args)  =
    (checkType l (tParam, checkExpr ht arg);
     checkArgs l ht params args)

(*the type of the invocation expression is the RETURN TYPE of the function...*)
and checkInvocation l ht id args =
    let
        val _ = (case HashTable.find decls id of
                     SOME MT_FUNC => MT_VOID
                   | SOME t => raise NotAFunctionException (l, t)
                   | NONE => raise NotAFunctionException (l, MT_VOID));
        (*We can do this safely since we already checked to
         * make sure f was a function*)
        val (FUNCTION {params=params, returnType=rt, ...}) =
            HashTable.lookup funcs id;
    in
        (checkArgs l ht params args;
         rt)
    end

and checkBinExpr ht opr lft rht l =
    let
        val tLft = checkExpr ht lft;
        val tRht = checkExpr ht rht;
    in
        (checkType l (tLft, tRht);
         case (opr, tLft) of
             (BOP_PLUS, MT_INT) => MT_INT
           | (BOP_MINUS, MT_INT) => MT_INT
           | (BOP_TIMES, MT_INT) => MT_INT
           | (BOP_DIVIDE, MT_INT) => MT_INT
           | (BOP_MOD, MT_INT) => MT_INT
           | (BOP_PLUS, _) => raise BinOpException (l, opr, MT_INT)
           | (BOP_MINUS, _) => raise BinOpException (l, opr, MT_INT)
           | (BOP_TIMES, _) => raise BinOpException (l, opr, MT_INT)
           | (BOP_DIVIDE, _) => raise BinOpException (l, opr, MT_INT)
           | (BOP_MOD, _) => raise BinOpException (l, opr, MT_INT)
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
           | (BOP_NE, _) => MT_BOOL)
    end

and checkExpr ht (EXP_NUM {value=n, ...}) = MT_INT
  | checkExpr ht (EXP_ID {id=s, line=l}) =
    (case HashTable.find ht s of
        SOME t => t
      | NONE => raise UndefException (l, s))
  | checkExpr ht (EXP_TRUE {...}) = MT_BOOL
  | checkExpr ht (EXP_FALSE {...}) = MT_BOOL
  | checkExpr ht EXP_UNDEFINED = MT_VOID (*Not sure about this...*)
  | checkExpr ht (EXP_BINARY {opr=opr, lft=lft, rht=rht, line=l}) =
    checkBinExpr ht opr lft rht l
  | checkExpr ht (EXP_UNARY {opr=opr, opnd=opnd, line=l}) =
    (case (opr, checkExpr ht opnd) of
         (UOP_NOT, MT_BOOL) => MT_BOOL
       | (UOP_MINUS, MT_INT) => MT_INT
       | (UOP_NOT, _) => raise UnOpException (l, opr, MT_BOOL)
       | (UOP_MINUS, _) => raise UnOpException (l, opr, MT_INT))
  | checkExpr ht (EXP_DOT {lft=lft, prop=prop, line=l}) =
    let
        val r = case checkExpr ht lft of
                    MT_STRUCT r => r
                  | t => raise NotAStructException (l, t);
        val rVars = HashTable.lookup types r;
        val tProp = checkLvalue rVars prop;
    in
        tProp
    end
  | checkExpr ht (EXP_NEW {id=s, line=l}) =
    (case HashTable.find types s of
         SOME _ => MT_STRUCT s
       | NONE => raise NotAStructException (l, MT_VOID))
  | checkExpr ht (EXP_INVOCATION {id=id, args=args, line=l}) =
    checkInvocation l ht id args
;

(*Returns a boolean indicating whether the statement returns on all paths*)
fun checkStmt rt ht (ST_BLOCK stmts) =
    checkStmts rt ht stmts
  | checkStmt rt ht (ST_ASSIGN {target=target, source=source, line=l}) =
    (checkType l (checkLvalue ht target, checkExpr ht source);
     false)
  | checkStmt rt ht (ST_PRINT {body=body, line=l, ...}) =
    (case checkExpr ht body of
         MT_INT => false
       | t => raise PrintException (l, t))
  | checkStmt rt ht (ST_READ lval) = false
  | checkStmt rt ht (ST_IF {guard=guard, thenBlk=thenBlk,
                            elseBlk=elseBlk, line=l}) =
    (case checkExpr ht guard of
         MT_BOOL => (checkStmt rt ht thenBlk) andalso (checkStmt rt ht elseBlk)
       | t => raise BooleanGuardException (l, t))
  | checkStmt rt ht (ST_WHILE {guard=guard, body=body, line=l}) =
    (checkStmt rt ht body;
     case checkExpr ht guard of
         MT_BOOL => false
       | t => raise BooleanGuardException (l, t))
  | checkStmt rt ht (ST_DELETE {exp=exp, line=_}) = false
  | checkStmt rt ht (ST_RETURN {exp=exp, line=l}) =
    (checkType l (rt, checkExpr ht exp);
     true)
  | checkStmt rt ht (ST_INVOCATION {id=id, args=args, line=l}) =
    (checkInvocation l ht id args;
     false)

and checkStmts1 retDetect rt ht [] = retDetect
  | checkStmts1 retDetect rt ht (stmt::stmts) =
    if checkStmt rt ht stmt then checkStmts1 true rt ht stmts
    else checkStmts1 retDetect rt ht stmts

and checkStmts rt ht L =
    checkStmts1 false rt ht L
;

fun checkFunc (FUNCTION {id=id, params=params, returnType=rt,
                         decls=ds, body=body, line=l}) =
    let
        val locals = HashTable.copy decls;
        val _ = app (fn (VAR_DECL {id=s, typ=t, ...}) =>
                        HashTable.insert locals (s, t)) ds;
        val _ = app (fn (VAR_DECL {id=s, typ=t, ...}) =>
                        HashTable.insert locals (s, t)) params;
        val retDetect = checkStmts rt locals body;
    in
        if retDetect = false andalso rt <> MT_VOID then
            raise NoReturnException l
        else ()
    end
;

fun addTypeDecl (TYPE_DECL {id=id, decls=ds, line=_}) =
    let
        val decls = makeHt ();
    in
        (HashTable.insert types (id, decls);
         app (fn (VAR_DECL {id=s, typ=t, ...}) =>
                 HashTable.insert decls (s, t)) ds
        )
    end
;

fun checkForMain1 mainDetect [] = mainDetect
  | checkForMain1 mainDetect ((s, FUNCTION {params=params,
                                            returnType=rt, ...})::funcs) =
    if s = "main" andalso length params = 0 andalso rt = MT_INT then
        checkForMain1 true funcs
    else checkForMain1 mainDetect funcs
;

fun checkForMain () =
    case HashTable.find funcs "main" of
        SOME (FUNCTION {params=params, returnType=rt, ...}) =>
        if length params = 0 andalso rt = MT_INT then ()
        else raise NoMainException 1
      | NONE => raise NoMainException 1
;

fun staticCheck file (PROGRAM {types=ts, decls=ds, funcs=fs}) =
    (app addTypeDecl ts;
     app (fn (VAR_DECL {id=s, typ=t, ...}) =>
             HashTable.insert decls (s, t)) ds;
     app (fn (func as FUNCTION {id=id, ...}) =>
        (HashTable.insert decls (id, MT_FUNC);
         HashTable.insert funcs (id, func))) fs;
     app checkFunc fs;
     checkForMain ())
    handle BinOpException (line, opr, t) =>
           fail file line ("Operator " ^ (binOp2Str opr) ^ " requires an " ^
                           (typ2Str t) ^ "type.\n")
         | UnOpException (line, opr, t) =>
           fail file line ("Operator " ^ (unOp2Str opr) ^ " requires an " ^
                           (typ2Str t) ^ "type.\n")
         | TypeMatchException (line, t1, t2) =>
           fail file line ("Types " ^ (typ2Str t1) ^ " and " ^ (typ2Str t2) ^
                           " do not match.\n")
         | NotAFunctionException (line, t) =>
           fail file line ("Type " ^ (typ2Str t) ^ " is not callable.\n")
         | NotAStructException (line, t) =>
           fail file line ("Expression requires a struct type (Supplied " ^
                           (typ2Str t) ^ ").\n")
         | PrintException (line, t) =>
           fail file line ("`print` requires an integer argument (Supplied " ^
                           (typ2Str t) ^ ").\n")
         | BooleanGuardException (line, t) =>
           fail file line ("Statement requires a boolean " ^
                           "expression (Supplied " ^ (typ2Str t) ^ ").\n")
         | UndefException (line, id) =>
           fail file line ("Undefined variable " ^ id ^ ".\n")
         | InvocationException line =>
           fail file line "Bad function invocation.\n"
         | NoReturnException line =>
           fail file line "Function does not return on all paths.\n"
         | NoMainException line =>
           fail file line "No function matching `main` signature.\n"
;

end;
