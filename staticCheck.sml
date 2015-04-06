use "json2AST.sml";

exception UndefException of int option * string; (*Fix this later*)
exception BinOpException of int * binaryOperator * miniType;
exception UnOpException of int * unaryOperator * miniType;
exception TypeMatchException of int * miniType * miniType;
exception NotAFunctionException of int * miniType;
exception NotAStructException of int * miniType;
exception PrintException of int * miniType;
exception BooleanGuardException of int * miniType;
exception ReturnTypeException of int * miniType * miniType;
exception NoReturnException of int;
exception InvocationException of int; (*I'll add arguments to this later.*)

fun makeHt () = HashTable.mkTable (HashString.hashString, op =) (10, Fail "");

(*GLOBALS*)
(* Variables and functions are in the same namespace,
 * but structure names are in a DIFFERENT namespace.*)
val types : (string, (string, miniType) HashTable.hash_table)
                HashTable.hash_table = makeHt ();
val decls : (string, miniType) HashTable.hash_table = makeHt ();
val funcs : (string, function) HashTable.hash_table = makeHt ();

(*HELPER FUNCTIONS*)

fun fail fileName line message =
    let
        val s = case line of SOME n => (Int.toString n) ^ ":" | NONE => "";
    in
        (TextIO.output (TextIO.stdErr, fileName ^ ":" ^ s ^ message);
         OS.Process.exit OS.Process.failure)
    end
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

fun miniType2Str MT_INT = "integer"
  | miniType2Str MT_VOID = "undefined"
  | miniType2Str MT_BOOL = "boolean"
  | miniType2Str MT_FUNC = "function"
  | miniType2Str (MT_STRUCT s) = "struct " ^ s
;

fun requireStruct l (t as MT_STRUCT _) = t
  | requireStruct l t = raise NotAStructException (l, t)
;

fun checkType l (MT_INT, MT_INT) = ()
  | checkType l (MT_BOOL, MT_BOOL) = ()
  | checkType l (_, MT_VOID) = () (*This says MT_VOID is equal to anything... not sure...*)
  | checkType l (t1 as MT_STRUCT s1, t2 as MT_STRUCT s2) =
    if s1 = s2 then () else raise TypeMatchException (l, t1, t2)
  | checkType l (t1, t2) = raise TypeMatchException (l, t1, t2)
;

fun checkLvalue ht (LVALUE lval) =
    case HashTable.find ht lval of
        SOME t => t
      | NONE => raise UndefException (NONE, lval)
;

fun checkArgs l ht (x::xs) [] = raise InvocationException l
  | checkArgs l ht [] (x::xs) = raise InvocationException l
  | checkArgs l ht (arg::args) (VAR_DECL {typ=tParam, ...}::params) =
    (checkType l (checkExpr ht arg, tParam);
     checkArgs l ht args params)

(*the type of the invocation expression is the RETURN TYPE of the function...*)
and checkInvocation l ht id args =
    let
        val _ = (case HashTable.find decls id of
                     SOME MT_FUNC => MT_VOID
                   | SOME t => raise NotAFunctionException (l, t)
                   | NONE => raise NotAFunctionException (l, MT_VOID));
        (*We can so this safely since we already checked to
         * make sure f was a function*)
        val (FUNCTION {params=params, returnType=rt, ...}) =
            HashTable.lookup funcs id;
    in
        ((* checkArgs l ht args params; *)
         rt)
    end

(*This should return the type of the expression*)
and checkExpr ht (EXP_NUM {value=n, ...}) = MT_INT
  | checkExpr ht (EXP_ID {id=s, line=l}) =
    (case HashTable.find ht s of
        SOME t => t
      | NONE => raise UndefException (SOME l, s))
  | checkExpr ht (EXP_TRUE {...} | EXP_FALSE {...}) = MT_BOOL
  | checkExpr ht EXP_UNDEFINED = MT_VOID (*Not sure about this...*)
  | checkExpr ht (EXP_BINARY {opr=opr, lft=lft, rht=rht, line=l}) =
    let
        val tLft = checkExpr ht lft;
        val tRht = checkExpr ht rht;
    in
        (checkType l (tLft, tRht);
         case opr of
             (BOP_PLUS | BOP_MINUS | BOP_TIMES | BOP_DIVIDE | BOP_MOD) =>
             (case tLft of
                  MT_INT => MT_INT
                | _ => raise BinOpException (l, opr, MT_INT))
           | (BOP_LT | BOP_GT | BOP_LE | BOP_GE) =>
             (case tLft of
                  MT_INT => MT_BOOL
                | _ => raise BinOpException (l, opr, MT_INT))
           | (BOP_AND | BOP_OR) =>
             (case tLft of
                  MT_BOOL => MT_BOOL
                | _ => raise BinOpException (l, opr, MT_BOOL))
           | (BOP_EQ | BOP_NE) => MT_BOOL
        )
    end
  | checkExpr ht (EXP_UNARY {opr=opr, opnd=opnd, line=l}) =
    let
        val tOpnd = checkExpr ht opnd;
    in
        case (opr, tOpnd) of
            (UOP_NOT, MT_BOOL) => MT_BOOL
          | (UOP_MINUS, MT_INT) => MT_INT
          | (UOP_NOT, _) => raise UnOpException (l, opr, MT_BOOL)
          | (UOP_MINUS, _) => raise UnOpException (l, opr, MT_INT)
    end
  | checkExpr ht (EXP_DOT {lft=lft, prop=prop, line=l}) =
    let
        val (MT_STRUCT r) = requireStruct l (checkExpr ht lft);
        val rVars = HashTable.lookup types r;
        val tProp = checkLvalue rVars prop;
    in
        tProp
    end
  | checkExpr ht (EXP_NEW {id=s, line=l}) =
    requireStruct l (HashTable.lookup ht s)
  | checkExpr ht (EXP_INVOCATION {id=id, args=args, line=l}) =
    checkInvocation l ht id args
;

fun checkStmt rt ht (ST_BLOCK stmts) =
    app (checkStmt rt ht) stmts
  | checkStmt rt ht (ST_ASSIGN {target=target, source=source, line=l}) =
    let
        val tTarget = checkLvalue ht target;
        val tSource = checkExpr ht source;
    in
        checkType l (tTarget, tSource)
    end
  | checkStmt rt ht (ST_PRINT {body=body, line=l, ...}) =
    (case checkExpr ht body of
         MT_INT => ()
       | t => raise PrintException (l, t))
  | checkStmt rt ht (ST_READ lval) =
    ()
  | checkStmt rt ht (ST_IF {guard=guard, thenBlk=thenBlk,
                            elseBlk=elseBlk, line=l}) =
    (checkStmt rt ht thenBlk;
     checkStmt rt ht elseBlk;
     case checkExpr ht guard of
         MT_BOOL => ()
       | t => raise BooleanGuardException (l, t)
    )
  | checkStmt rt ht (ST_WHILE {guard=guard, body=body, line=l}) =
    (checkStmt rt ht body;
     case checkExpr ht guard of
         MT_BOOL => ()
       | t => raise BooleanGuardException (l, t)
    )
  | checkStmt rt ht (ST_DELETE {exp=exp, line=_}) =
    ()
  | checkStmt rt ht (ST_RETURN {exp=exp, line=l}) =
    checkType l (rt, checkExpr ht exp)
  | checkStmt rt ht (ST_INVOCATION {id=id, args=args, line=l}) =
    (checkInvocation l ht id args;
     ())
    handle TypeMatchException t=>
           raise ReturnTypeException t
;

fun checkFunc (FUNCTION {id=id, params=params, returnType=rt,
                         decls=ds, body=body, line=_}) =
    let
        val locals = HashTable.copy decls;
    in
        (app (fn (VAR_DECL {id=s, typ=t, ...}) =>
                 HashTable.insert locals (s, t)) ds;
         app (fn (VAR_DECL {id=s, typ=t, ...}) =>
                 HashTable.insert locals (s, t)) params;
         app (checkStmt rt locals) body
        )
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
  
fun staticCheck file =
    let
        val PROGRAM {decls=ds, funcs=fs, types=ts} = json2Ast file;
    in
        (app addTypeDecl ts;
         app (fn (VAR_DECL {id=s, typ=t, ...}) =>
                 HashTable.insert decls (s, t)) ds;
         app (fn (func as FUNCTION {id=id, ...}) =>
                 (HashTable.insert decls (id, MT_FUNC);
                  HashTable.insert funcs (id, func))) fs;
         app checkFunc fs)
    end
    handle BinOpException (line, opr, t) =>
           fail file (SOME line) ("Operator " ^ (binOp2Str opr) ^ " requires an " ^
                                  (miniType2Str t) ^ "type.\n")
         | UnOpException (line, opr, t) =>
           fail file (SOME line) ("Operator " ^ (unOp2Str opr) ^ " requires an " ^
                                  (miniType2Str t) ^ "type.\n")
         | TypeMatchException (line, t1, t2) =>
           fail file (SOME line) ("Types " ^ (miniType2Str t1) ^
                                  " and " ^ (miniType2Str t2) ^
                                  " do not match.\n")
         | NotAFunctionException (line, t) =>
           fail file (SOME line) ("Type " ^ (miniType2Str t) ^
                                  " is not callable.\n")
         | NotAStructException (line, t) =>
           fail file (SOME line) ("Expression requires a struct type (Supplied " ^
                                  (miniType2Str t) ^ ").\n")
         | PrintException (line, t) =>
           fail file (SOME line) ("`print` requires an integer argument (Supplied " ^
                                  (miniType2Str t) ^ ").\n")
         | BooleanGuardException (line, t) =>
           fail file (SOME line) ("Statement requires a boolean expression (Supplied " ^
                                  (miniType2Str t) ^ ").\n")
         | UndefException (line, id) =>
           fail file line ("Undefined variable " ^ id ^ ".\n")
         | InvocationException line =>
           fail file (SOME line) "Bad function invocation.\n"
;

staticCheck "tests/1.json";
staticCheck "tests/2.json";
staticCheck "tests/ret.json";
