use "json2AST.sml";

fun makeHt () = HashTable.mkTable (HashString.hashString, op =) (10, Fail "");

(*
SO WE HAVE TO EXCEPT MINITYPE TO INCLUDE FUNCTIONS
*)


exception BinOpException of binaryOperator * miniType;
exception UnOpException of unaryOperator * miniType;
exception TypeMatchException of miniType * miniType;
exception NotAFunctionException of string

(*GLOBALS*)
val types : (string, varDecl list) HashTable.hash_table = makeHt ();
val decls : (string, miniType) HashTable.hash_table = makeHt ();
val funcs : (string, function) HashTable.hash_table = makeHt ();

(*
NOTE
Variables and functions are in the same namespace.
BUT, structure names are in a DIFFERENT namespace.
*)

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
  | miniType2Str (MT_STRUCT s) = "struct " ^ s
;

fun checkType (MT_INT, MT_INT) = ()
  | checkType (MT_BOOL, MT_BOOL) = ()
  | checkType (MT_VOID, _) = () (*There two lines say MT_VOID is equal to anything... not sure...*)
  | checkType (_, MT_VOID) = ()
  | checkType (t1 as MT_STRUCT s1, t2 as MT_STRUCT s2) =
    if s1 = s2 then () else raise TypeMatchException (t1, t2)
  | checkType (t1, t2) = raise TypeMatchException (t1, t2)
;

fun checkLvalue ht (LVALUE lval) =
    case HashTable.find ht lval of
        SOME t => t
      | NONE => raise Fail ("Undefined variable " ^ lval ^ ".")
;

(*This should return the type of the expression*)
fun checkExpr ht (EXP_NUM n) = MT_INT
  | checkExpr ht (EXP_ID s) =
    (case HashTable.find ht s of
        SOME t => t
      | NONE => raise Fail ("Undefined variable " ^ s ^ "."))
  | checkExpr ht (EXP_TRUE | EXP_FALSE) = MT_BOOL
  | checkExpr ht EXP_UNDEFINED = MT_VOID (*Not sure about this...*)
  | checkExpr ht (EXP_BINARY {opr=opr, lft=lft, rht=rht}) =
    let
        val tLft = checkExpr ht lft;
        val tRht = checkExpr ht rht;
    in
        (checkType (tLft, tRht);
         case opr of
             (BOP_PLUS | BOP_MINUS | BOP_TIMES | BOP_DIVIDE | BOP_MOD |
              BOP_LT | BOP_GT | BOP_LE | BOP_GE) =>
             (case tLft of
                  MT_INT => MT_INT
                | _ => raise BinOpException (opr, MT_INT))
           | (BOP_AND | BOP_OR) =>
             (case tLft of
                  MT_BOOL => MT_BOOL
                | _ => raise BinOpException (opr, MT_BOOL))
           | (BOP_EQ | BOP_NE) => tLft
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
        val tProp = checkLvalue ht prop;
        val tLft = checkExpr ht lft;
    in
        MT_VOID
       (*Here is where we check the declared types to make sure the struct actually has that field
       [Exception if we don't]. Then we return the type of that field*)
    end
  | checkExpr ht (EXP_NEW s) =
    MT_VOID
  | checkExpr ht (EXP_INVOCATION {id=id, args=args}) =
    (case HashTable.find funcs id of
        SOME _ => MT_VOID
      | NONE => raise NotAFunctionException id)
;

fun checkStmt ht (ST_BLOCK stmts) =
    app (checkStmt ht) stmts
  | checkStmt ht (ST_ASSIGN {target=target, source=source}) =
    ()
  | checkStmt ht (ST_PRINT {body=body, endl=endl}) =
    ()
  | checkStmt ht (ST_READ lval) =
    ()
  | checkStmt ht (ST_IF {guard=guard, thenBlk=thenBlk, elseBlk=elseBlk}) =
    ()
  | checkStmt ht (ST_WHILE {guard=guard, body=body}) =
    ()
  | checkStmt ht (ST_DELETE exp) =
    ()
  | checkStmt ht (ST_RETURN exp) =
    ()
  | checkStmt ht (ST_INVOCATION {id=id, args=args}) =
    ()
;

fun checkFunc (FUNCTION {id=id, params=params, returnType=rt,
                         decls=decls, body=body}) =
    let
        val localVars : (string, miniType) HashTable.hash_table = makeHt ();
    in
        print (id ^ "\n");
    end
;

fun staticCheck file =
    let
        val PROGRAM {decls=ds, funcs=fs, types=ts} = json2Ast file;
    in
        (app (fn (TYPE_DECL {id=s, decls=L}) =>
                 HashTable.insert types (s, L)) ts;
         app (fn (VAR_DECL (t, s)) =>
                 HashTable.insert decls (s, t)) ds;
         app (fn (func as FUNCTION {id=id, ...}) =>
                 HashTable.insert funcs (id, func)) fs;
         app checkFunc fs)
    end
    handle BinOpException (opr, t) =>
           fail ("Operator " ^ (binOp2Str opr) ^ " requires an " ^
                 (miniType2Str t) ^ "type.")
         | UnOpException (opr, t) =>
           fail ("Operator " ^ (unOp2Str opr) ^ " requires an " ^
                 (miniType2Str t) ^ "type.")
         | TypeMatchException (t1, t2) =>
           fail ("Types " ^ (miniType2Str t1) ^ " and " ^
                 (miniType2Str t2) ^ " do not match.")
         | NotAFunctionException id => fail (id ^ " is not a function.")
;

staticCheck "tests/1.json";
