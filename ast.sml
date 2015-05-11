structure Ast = struct

datatype binary_operator =
     BOP_PLUS
   | BOP_MINUS
   | BOP_TIMES
   | BOP_DIVIDE
   | BOP_EQ
   | BOP_NE
   | BOP_LT
   | BOP_GT
   | BOP_LE
   | BOP_GE
   | BOP_AND
   | BOP_OR


datatype unary_operator = UOP_NOT | UOP_MINUS


datatype typ =
     MT_VOID
   | MT_INT
   | MT_BOOL
   | MT_FUNC
   | MT_STRUCT of string


datatype var_decl = VAR_DECL of {id: string, typ: typ, line: int}


datatype type_decl = TYPE_DECL of {id: string, decls: var_decl list, line: int}


datatype lvalue =
     LV_ID of {id: string, line: int}
   | LV_DOT of {lft: lvalue, prop:string, line:int}


datatype expression =
     EXP_NUM of {value: int, line: int}
   | EXP_ID of {id: string, line: int}
   | EXP_TRUE of {line: int}
   | EXP_FALSE of {line: int}
   | EXP_NULL of {line: int}
   | EXP_BINARY of {opr: binary_operator, lft: expression,
                    rht: expression, line: int}
   | EXP_UNARY of {opr: unary_operator, opnd: expression, line: int}
   | EXP_DOT of {lft: expression, prop: string, line: int}
   | EXP_NEW of {id: string, line: int}
   | EXP_INVOCATION of {id: string, args: expression list, line: int}


datatype statement =
     ST_BLOCK of statement list
   | ST_ASSIGN of {target: lvalue, source: expression, line: int}
   | ST_PRINT of {body: expression, endl: bool, line: int}
   | ST_READ of {id: lvalue, line: int}
   | ST_IF of {guard: expression, thenBlk: statement,
               elseBlk: statement, line: int}
   | ST_WHILE of {guard: expression, body: statement, line: int}
   | ST_DELETE of {exp: expression, line: int}
   | ST_RETURN of {exp: expression option, line: int}
   | ST_INVOCATION of {id: string, args: expression list, line: int}


datatype function =
     FUNCTION of {
         id: string,
         params: var_decl list,
         returnType: typ,
         decls: var_decl list,
         body: statement list,
         line: int
     }


datatype program =
     PROGRAM of {
         types: type_decl list,
         decls: var_decl list,
         funcs: function list
     }


val binOpToStr =
 fn BOP_PLUS => " + "
  | BOP_MINUS => " - "
  | BOP_TIMES => " * "
  | BOP_DIVIDE => " / "
  | BOP_EQ => " == "
  | BOP_NE => " != "
  | BOP_LT => " < "
  | BOP_GT => " > "
  | BOP_LE => " <= "
  | BOP_GE => " >= "
  | BOP_AND => " && "
  | BOP_OR => " || "


val unOpToStr = fn UOP_NOT => "!"
  | UOP_MINUS => "-"


val typeToStr = fn MT_VOID => "void"
  | MT_INT => "int"
  | MT_BOOL => "bool"
  | MT_FUNC => ""
  | (MT_STRUCT s) => "struct " ^ s


val varDeclToStr = fn VAR_DECL {id=id, typ=t, ...} => (typeToStr t) ^ " " ^ id


fun typeDeclToStr (TYPE_DECL {id=id, decls=decls, ...}) =
    "struct " ^ id ^ "\n{\n" ^
    (foldr (fn (d, s) => (varDeclToStr d) ^ ";\n" ^ s) "" decls) ^ "};"


fun lvalueToStr (LV_ID {id=id, ...}) = id
  | lvalueToStr (LV_DOT {lft=lft, prop=prop, ...}) =
    (lvalueToStr lft) ^ "." ^ prop


fun expToStr (EXP_NUM {value=n, ...}) = Int.toString n
  | expToStr (EXP_ID {id=id, ...}) = id
  | expToStr (EXP_TRUE _) = "true"
  | expToStr (EXP_FALSE _) = "false"
  | expToStr (EXP_NULL _) = "null"
  | expToStr (EXP_BINARY {opr=opr, lft=lft, rht=rht, ...}) =
    (expToStr lft) ^ (binOpToStr opr) ^ (expToStr rht)
  | expToStr (EXP_UNARY {opr=opr, opnd=opnd, ...}) =
    (unOpToStr opr) ^ (expToStr opnd)
  | expToStr (EXP_DOT {lft=lft, prop=prop, ...}) = (expToStr lft) ^ "." ^ prop
  | expToStr (EXP_NEW {id=s, ...}) = "new " ^ s
  | expToStr (EXP_INVOCATION {id=id, args=args, ...}) =
    id ^ "(" ^ (Util.foldd ", " expToStr args) ^ ")"


fun stmtToStr (ST_BLOCK body) =
    "{\n" ^ (foldr (fn (t, s) => (stmtToStr t) ^ ";\n") "" body) ^ "}"
  | stmtToStr (ST_ASSIGN {target=target, source=source, ...}) =
    (lvalueToStr target) ^ " = " ^ (expToStr source)
  | stmtToStr (ST_PRINT {body=body, endl=endl, ...}) =
    "print " ^ (expToStr body) ^ (if endl then " endl" else "")
  | stmtToStr (ST_READ {id=l, ...}) = "read " ^ (lvalueToStr l)
  | stmtToStr (ST_IF {guard=guard, thenBlk=thenBlk, elseBlk=elseBlk, ...}) =
    "if (" ^ (expToStr guard) ^ ")\n" ^ (stmtToStr thenBlk)  ^
    " else " ^ (stmtToStr elseBlk)
  | stmtToStr (ST_WHILE {guard=guard, body=body, ...}) =
    "while (" ^ (expToStr guard) ^ ")\n" ^ (stmtToStr body)
  | stmtToStr (ST_DELETE {exp=exp, ...}) = "delete " ^ (expToStr exp)
  | stmtToStr (ST_RETURN {exp=exp, ...}) =
    "return" ^ (case exp of SOME e => expToStr e | NONE => "")
  | stmtToStr (ST_INVOCATION {id=id, args=args, ...}) =
    id ^ "(" ^ (Util.foldd ", " expToStr args) ^ ")"


fun funcToStr (FUNCTION {id=id, params=params, returnType=rt, decls=decls,
                         body=body, ...}) =
    "fun " ^ id ^ " (" ^ (Util.foldd ", " varDeclToStr params) ^ ") " ^
    (typeToStr rt) ^ "\n{\n" ^
    (foldr (fn (d, s) => (varDeclToStr d) ^ ";\n" ^ s) "" decls) ^
    (foldr (fn (t, s) => (stmtToStr t) ^ "\n" ^ s) "" body) ^ "}\n"


fun programToStr (PROGRAM {types=types, decls=decls, funcs=funcs}) =
    (foldr (fn (t, s) => (typeDeclToStr t) ^ "\n" ^ s) "" types) ^ "\n" ^
    (foldr (fn (v, s) => (varDeclToStr v) ^ "\n" ^ s) "" decls) ^ "\n" ^
    (foldr (fn (f, s) => (funcToStr f) ^ "\n" ^ s) "" funcs) ^ "\n"

end
