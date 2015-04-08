open TextIO;

structure printAST :
          sig
              val printAST : program -> unit
          end
= struct

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

fun printMiniType MT_VOID = "void"
  | printMiniType MT_INT = "int"
  | printMiniType MT_BOOL = "bool"
  | printMiniType MT_FUNC = ""
  | printMiniType (MT_STRUCT s) = "struct " ^ s
;

fun printVarDecl (VAR_DECL {id=s, typ=t, line=_}) =
    (printMiniType t) ^ " " ^ s;
;

fun printNestedDecls ds = foldr (fn (d, s) => (printVarDecl d) ^ ";\n" ^ s)
                                "" ds;

fun printTypeDecl (TYPE_DECL {id=id, decls=decls, line=_}) =
    "struct " ^ id ^ "\n{\n" ^ (printNestedDecls decls) ^ "};"
;

fun printLvalue (LV_ID {id=s, ...}) = s
  | printLvalue  (LV_DOT {lft=lft, prop=prop, ...}) =
    (printLvalue lft) ^ "." ^ prop
;

fun printExpression (EXP_NUM {value=n, line=_}) = Int.toString n
  | printExpression (EXP_ID {id=id, line=_}) = id
  | printExpression (EXP_TRUE {...}) = "true"
  | printExpression (EXP_FALSE {...}) = "false"
  | printExpression EXP_UNDEFINED = "null"
  | printExpression (EXP_BINARY {opr=opr, lft=lft, rht=rht, line=_}) =
    (printExpression lft) ^ (printBinaryOpr opr) ^ (printExpression rht)
  | printExpression (EXP_UNARY {opr=opr, opnd=opnd, line=_}) =
    (printUnaryOpr opr) ^ (printExpression opnd)
  | printExpression (EXP_DOT {lft=lft, prop=prop, line=_}) =
    (printExpression lft) ^ "." ^ prop
  | printExpression (EXP_NEW {id=s, line=_}) =
    "new " ^ s
  | printExpression (EXP_INVOCATION {id=id, args=args, line=_}) =
    id ^ "(" ^ (printArgs args) ^ ")"

and printStatement (ST_BLOCK body) =
    "{\n" ^ (foldr (fn (t, s) => (printStatement t) ^ ";\n") "" body) ^ "}"
  | printStatement (ST_ASSIGN {target=target, source=source, line=_}) =
    (printLvalue target) ^ " = " ^ (printExpression source)
  | printStatement (ST_PRINT {body=body, endl=endl, line=_}) =
    "print " ^ (printExpression body) ^ (if endl then " endl" else "")
  | printStatement (ST_READ {id=l, line=_}) =
    "read " ^ (printLvalue l)
  | printStatement (ST_IF {guard=guard, thenBlk=thenBlk,
                           elseBlk=elseBlk, line=_}) =
    "if (" ^ (printExpression guard) ^ ")\n" ^ (printStatement thenBlk)  ^
    " else " ^ (printStatement elseBlk)
  | printStatement (ST_WHILE {guard=guard, body=body, line=_}) =
    "while (" ^ (printExpression guard) ^ ")\n" ^ (printStatement body)
  | printStatement (ST_DELETE {exp=exp, line=_}) =
    "delete " ^ (printExpression exp)
  | printStatement (ST_RETURN {exp=exp, line=_}) =
    "return " ^ (printExpression exp)
  | printStatement (ST_INVOCATION {id=id, args=args, line=_}) =
    id ^ "(" ^ (printArgs args) ^ ")"

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

fun printBody b = foldr (fn (t, s) => (printStatement t) ^ "\n" ^ s) "" b;

fun printFunc (FUNCTION {id=id, params=params, returnType=rt, decls=decls,
                         body=body, line=_}) =
    let
        val _ = print (id ^ "BODY LEN " ^ (Int.toString (length body)) ^ "\n");
    in
        ("fun " ^ id ^ " (" ^ (printParams params) ^ ") " ^ (printMiniType rt) ^
         "\n{\n" ^ (printNestedDecls decls) ^ (printBody body) ^ "}\n")
    end
;

fun printAST (PROGRAM {types=types, decls=decls, funcs=funcs}) =
    print ((foldr (fn (t, s) => (printTypeDecl t) ^ "\n" ^ s) "" types) ^ "\n" ^
           (foldr (fn (v, s) => (printVarDecl v) ^ "\n" ^ s) "" decls) ^ "\n" ^
           (foldr (fn (f, s) => (printFunc f) ^ "\n" ^ s) "" funcs) ^ "\n")
;

end;
