open TextIO;
use "json2Ast.sml";

fun printBinaryOpr BOP_PLUS = " + "
  | printBinaryOpr BOP_MINUS = " - "
  | printBinaryOpr BOP_TIMES = " * "
  | printBinaryOpr BOP_DIVIDE = " / "
  | printBinaryOpr BOP_MOD = " % "
  | printBinaryOpr BOP_EQ = " == "
  | printBinaryOpr BOP_NE = " != "
  | printBinaryOpr BOP_LT = " < "
  | printBinaryOpr BOP_GT = " > "
  | printBinaryOpr BOP_LE = " <= "
  | printBinaryOpr BOP_GE = " >= "
  | printBinaryOpr BOP_AND = " && "
  | printBinaryOpr BOP_OR = " || "
;

fun printUnaryOpr UOP_NOT = "!"
  | printUnaryOpr UOP_MINUS = "-"
;

fun printMiniType T_VOID = ">>>VOID<<<"
  | printMiniType T_INT = "int"
  | printMiniType T_BOOL = "bool"
  | printMiniType (T_STRUCT s) = "struct " ^ s
;

fun printVarDecl (VAR_DECL (t, s)) =
    (printMiniType t) ^ " " ^ s;
;

fun printNestedVarDecls [] = ""
  | printNestedVarDecls (decl::decls) =
    (printVarDecl decl) ^ ";\n" ^ (printNestedVarDecls decls)
;

fun printTypeDecl (TYPE_DECL {id=id, decls=decls}) =
    "struct " ^ id ^ "\n{\n" ^ (printNestedVarDecls decls) ^ "};"
;

fun printLvalue (LVALUE l) = l;
  
fun printExpression (EXP_NUM n) = Int.toString n
  | printExpression (EXP_ID id) = id
  | printExpression EXP_TRUE = "true"
  | printExpression EXP_FALSE = "false"
  | printExpression EXP_UNDEFINED = "null"
  | printExpression (EXP_BINARY {opr=opr, lft=lft, rht=rht}) =
    (printExpression lft) ^ (printBinaryOpr opr) ^ (printExpression rht)
  | printExpression (EXP_UNARY {opr=opr, opnd=opnd}) =
    (printUnaryOpr opr) ^ (printExpression opnd)
  | printExpression (EXP_ACCESS {lft=lft, prop=prop}) =
    (printExpression lft) ^ "." ^ (printLvalue prop)
  | printExpression (EXP_NEW s) =
    "new " ^ s
  | printExpression (EXP_INVOKE {id=id, args=args}) =
    id ^ "(" ^ (printArgs args) ^ ")"

and printStatement (ST_BLOCK body) =
    "{\n" ^ (printBody body) ^ "}"
  | printStatement (ST_ASSIGN {target=target, source=source}) =
    (printLvalue target) ^ " = " ^ (printExpression source)
  | printStatement (ST_PRINT {body=body, endl=endl}) =
    "print " ^ (printExpression body) ^ (if endl then " endl" else "")
  | printStatement (ST_READ l) =
    "read " ^ (printLvalue l)
  | printStatement (ST_COND {guard=guard, thenBlk=thenBlk, elseBlk=elseBlk}) =
    "if (" ^ (printExpression guard) ^ ")\n" ^ (printStatement thenBlk)
  | printStatement (ST_LOOP {guard=guard, body=body}) =
    "ST_LOOP"
  | printStatement (ST_DELETE exp) =
    "delete " ^ (printExpression exp)
  | printStatement (ST_RETURN exp) =
    "return " ^ (printExpression exp)
  | printStatement (ST_INVOKE {id=id, args=args}) =
    id ^ "(" ^ (printArgs args) ^ ")"

and printBody [] = ""
  | printBody (stmt::stmts) =
    (printStatement stmt) ^ ";\n" ^ (printBody stmts)

and printArgs [] = ""
  | printArgs (exp::[]) = printExpression exp
  | printArgs (exp::exps) =
    (printExpression exp) ^ ", " ^ (printArgs exps)
;
  
fun printParams [] = ""
  | printParams (decl::[]) = printVarDecl decl
  | printParams (decl::decls) =
    (printVarDecl decl) ^ ", " ^ (printParams decls)
;

fun printFunc (FUNCTION {id=id, params=params, returnType=rt, decls=decls,
                         body=body}) =
    "fun " ^ id ^ " (" ^ (printParams params) ^ ") " ^ (printMiniType rt) ^
    "\n{\n" ^ (printNestedVarDecls decls) ^ (printBody body) ^ "}\n"
;
  
fun printFuncs [] = ""
  | printFuncs (func::funcs) =
    (printFunc func) ^ "\n" ^ (printFuncs funcs)
;

fun printVarDecls [] = ""
  | printVarDecls (decl::decls) =
    (printVarDecl decl) ^ ";\n" ^ (printVarDecls decls)
;
  
fun printTypeDecls [] = ""
  | printTypeDecls (decl::decls) =
    (printTypeDecl decl) ^ "\n" ^ (printTypeDecls decls)
;

fun printAst (PROGRAM {types=types, decls=decls, funcs=funcs}) =
    print ((printTypeDecls types) ^ "\n" ^
           (printVarDecls decls) ^ "\n" ^
           (printFuncs funcs))
;

printAst (json2Ast "tests/1.json");
