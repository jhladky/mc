use "json2AST.sml";

exception UndefException of string;
exception BinOpException of binaryOperator * miniType;
exception UnOpException of unaryOperator * miniType;
exception TypeMatchException of miniType * miniType;
exception NotAFunctionException of miniType;
exception NotAStructException of miniType;
exception PrintException of miniType;
exception BooleanGuardException of miniType;
exception NoReturnException;
exception InvocationException; (*I'll add arguments to this later.*)
exception ReturnTypeException of miniType * miniType;

fun makeHt () = HashTable.mkTable (HashString.hashString, op =) (10, Fail "");

(*GLOBALS*)
(* Variables and functions are in the same namespace,
 * but structure names are in a DIFFERENT namespace.*)
val types : (string, (string, miniType) HashTable.hash_table)
                HashTable.hash_table = makeHt ();
val decls : (string, miniType) HashTable.hash_table = makeHt ();
val funcs : (string, function) HashTable.hash_table = makeHt ();

(*HELPER FUNCTIONS*)

fun fail str = (
    TextIO.output (TextIO.stdErr, str);
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

fun miniType2Str MT_INT = "integer"
  | miniType2Str MT_VOID = "undefined"
  | miniType2Str MT_BOOL = "boolean"
  | miniType2Str MT_FUNC = "function"
  | miniType2Str (MT_STRUCT s) = "struct " ^ s
;

fun requireStruct (t as MT_STRUCT _) = t
  | requireStruct t = raise NotAStructException t
;

fun checkType (MT_INT, MT_INT) = ()
  | checkType (MT_BOOL, MT_BOOL) = ()
  | checkType (_, MT_VOID) = () (*This says MT_VOID is equal to anything... not sure...*)
  | checkType (t1 as MT_STRUCT s1, t2 as MT_STRUCT s2) =
    if s1 = s2 then () else raise TypeMatchException (t1, t2)
  | checkType (t1, t2) = raise TypeMatchException (t1, t2)
;

fun checkLvalue ht (LVALUE lval) =
    case HashTable.find ht lval of
        SOME t => t
      | NONE => raise UndefException lval
;

fun checkArgs ht (x::xs) [] = raise InvocationException
  | checkArgs ht [] (x::xs) = raise InvocationException
  | checkArgs ht (arg::args) (VAR_DECL (tParam, _)::params) =
    (checkType (checkExpr ht arg, tParam);
     checkArgs ht args params
    )

(*the type of the invocation expression is the RETURN TYPE of the function...*)
and checkInvocation ht id args =
    let
        val _ = (case HashTable.find decls id of
                     SOME MT_FUNC => MT_VOID
                   | SOME t => raise NotAFunctionException t
                   | NONE => raise NotAFunctionException MT_VOID);
        (*We can so this safely since we already checked to
         * make sure f was a function*)
        val (FUNCTION {params=params, returnType=rt, ...}) =
            HashTable.lookup funcs id;
    in
        rt
    end

(*This should return the type of the expression*)
and checkExpr ht (EXP_NUM n) = MT_INT
  | checkExpr ht (EXP_ID s) =
    (case HashTable.find ht s of
        SOME t => t
      | NONE => raise UndefException s)
  | checkExpr ht (EXP_TRUE | EXP_FALSE) = MT_BOOL
  | checkExpr ht EXP_UNDEFINED = MT_VOID (*Not sure about this...*)
  | checkExpr ht (EXP_BINARY {opr=opr, lft=lft, rht=rht}) =
    let
        val tLft = checkExpr ht lft;
        val tRht = checkExpr ht rht;
    in
        (checkType (tLft, tRht);
         case opr of
             (BOP_PLUS | BOP_MINUS | BOP_TIMES | BOP_DIVIDE | BOP_MOD) =>
             (case tLft of
                  MT_INT => MT_INT
                | _ => raise BinOpException (opr, MT_INT))
           | (BOP_LT | BOP_GT | BOP_LE | BOP_GE) =>
             (case tLft of
                  MT_INT => MT_BOOL
                | _ => raise BinOpException (opr, MT_INT))
           | (BOP_AND | BOP_OR) =>
             (case tLft of
                  MT_BOOL => MT_BOOL
                | _ => raise BinOpException (opr, MT_BOOL))
           | (BOP_EQ | BOP_NE) => MT_BOOL
        )
    end
  | checkExpr ht (EXP_UNARY {opr=opr, opnd=opnd}) =
    let
        val tOpnd = checkExpr ht opnd;
    in
        case (opr, tOpnd) of
            (UOP_NOT, MT_BOOL) => MT_BOOL
          | (UOP_MINUS, MT_INT) => MT_INT
          | (UOP_NOT, _) => raise UnOpException (opr, MT_BOOL)
          | (UOP_MINUS, _) => raise UnOpException (opr, MT_INT)
    end
  | checkExpr ht (EXP_DOT {lft=lft, prop=prop}) =
    let
        val (MT_STRUCT r) = requireStruct (checkExpr ht lft);
        val rVars = HashTable.lookup types r;
        val tProp = checkLvalue rVars prop;
    in
        tProp
    end
  | checkExpr ht (EXP_NEW s) =
    requireStruct (HashTable.lookup ht s)
  | checkExpr ht (EXP_INVOCATION {id=id, args=args}) =
    checkInvocation ht id args
;

fun checkStmt rt ht (ST_BLOCK stmts) =
    app (checkStmt rt ht) stmts
  | checkStmt rt ht (ST_ASSIGN {target=target, source=source}) =
    let
        val tTarget = checkLvalue ht target;
        val tSource = checkExpr ht source;
    in
        checkType (tTarget, tSource)
    end
  | checkStmt rt ht (ST_PRINT {body=body,...}) =
    (case checkExpr ht body of
         MT_INT => ()
       | t => raise PrintException t)
  | checkStmt rt ht (ST_READ lval) =
    ()
  | checkStmt rt ht (ST_IF {guard=guard, thenBlk=thenBlk, elseBlk=elseBlk}) =
    (checkStmt rt ht thenBlk;
     checkStmt rt ht elseBlk;
     case checkExpr ht guard of
         MT_BOOL => ()
       | t => raise BooleanGuardException t
    )
  | checkStmt rt ht (ST_WHILE {guard=guard, body=body}) =
    (checkStmt rt ht body;
     case checkExpr ht guard of
         MT_BOOL => ()
       | t => raise BooleanGuardException t
    )
  | checkStmt rt ht (ST_DELETE exp) =
    ()
  | checkStmt rt ht (ST_RETURN exp) =
    checkType (rt, checkExpr ht exp)
  | checkStmt rt ht (ST_INVOCATION {id=id, args=args}) =
    (checkInvocation ht id args;
     ())
    handle TypeMatchException p =>
           raise ReturnTypeException p
;

fun checkFunc (FUNCTION {id=id, params=params, returnType=rt,
                         decls=ds, body=body}) =
    let
        val locals = HashTable.copy decls;
    in
        (app (fn (VAR_DECL (t, s)) => HashTable.insert locals (s, t)) ds;
         app (fn (VAR_DECL (t, s)) => HashTable.insert locals (s, t)) params;
         app (checkStmt rt locals) body
        )
    end
;

fun addTypeDecl (TYPE_DECL {id=id, decls=ds}) =
    let
        val decls = makeHt ();
    in
        (HashTable.insert types (id, decls);
         app (fn (VAR_DECL (t, s)) =>
                 HashTable.insert decls (s, t)) ds
        )
    end
;

fun staticCheck file =
    let
        val PROGRAM {decls=ds, funcs=fs, types=ts} = json2Ast file;
    in
        (app addTypeDecl ts;
         app (fn (VAR_DECL (t, s)) =>
                 HashTable.insert decls (s, t)) ds;
         app (fn (func as FUNCTION {id=id, ...}) =>
                 (HashTable.insert decls (id, MT_FUNC);
                  HashTable.insert funcs (id, func))) fs;
         app checkFunc fs;
         print "OK\n"
        )
    end
    handle BinOpException (opr, t) =>
           fail ("Operator " ^ (binOp2Str opr) ^ " requires an " ^
                 (miniType2Str t) ^ "type.\n")
         | UnOpException (opr, t) =>
           fail ("Operator " ^ (unOp2Str opr) ^ " requires an " ^
                 (miniType2Str t) ^ "type.\n")
         | TypeMatchException (t1, t2) =>
           fail ("Types " ^ (miniType2Str t1) ^ " and " ^
                 (miniType2Str t2) ^ " do not match.\n")
         | NotAFunctionException t =>
           fail ("Type " ^ (miniType2Str t) ^ " is not callable.\n")
         | NotAStructException t =>
           fail ("Expression requires a struct type (Supplied " ^
                 (miniType2Str t) ^ ").\n")
         | PrintException t =>
           fail ("`print` requires an integer argument (Supplied " ^
                 (miniType2Str t) ^ ").\n")
         | BooleanGuardException t =>
           fail ("Statement requires a boolean expression (Supplied " ^
                 (miniType2Str t) ^ ").\n")
         | UndefException id =>
           fail ("Undefined variable " ^ id ^ ".\n")
         | InvocationException =>
           fail "Bad function invocation.\n"
;

staticCheck "tests/1.json";
staticCheck "tests/2.json";
staticCheck "tests/ret.json";
