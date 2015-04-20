structure Ast = struct

datatype binaryOperator =
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


datatype unaryOperator =
     UOP_NOT
   | UOP_MINUS


datatype miniType =
     MT_VOID
   | MT_INT
   | MT_BOOL
   | MT_FUNC
   | MT_STRUCT of string


datatype varDecl =
     VAR_DECL of {id: string, typ: miniType, line: int}


datatype typeDecl =
     TYPE_DECL of {id: string, decls: varDecl list, line: int}


datatype lvalue =
     LV_ID of {id: string, line: int}
   | LV_DOT of {lft: lvalue, prop:string, line:int}


datatype expression =
     EXP_NUM of {value: int, line: int}
   | EXP_ID of {id: string, line: int}
   | EXP_TRUE of {line: int}
   | EXP_FALSE of {line: int}
   | EXP_NULL
   | EXP_BINARY of {opr: binaryOperator, lft: expression,
                    rht: expression, line: int}
   | EXP_UNARY of {opr: unaryOperator, opnd: expression, line: int}
   | EXP_DOT of {lft: expression, prop: string, line: int}
   | EXP_NEW of {id: string, line: int}
   | EXP_INVOCATION of {id: string, args: expression list, line: int}


datatype statement =
     ST_BLOCK of statement list
   | ST_ASSIGN of {target: lvalue, source: expression, line: int}
   | ST_PRINT of {body: expression, endl: bool, line: int}
   | ST_READ of {id: lvalue, line: int}
   | ST_IF of {guard: expression, thenBlk: statement, elseBlk: statement, line: int}
   | ST_WHILE of {guard: expression, body: statement, line: int}
   | ST_DELETE of {exp: expression, line: int}
   | ST_RETURN of {exp: expression option, line: int}
   | ST_INVOCATION of {id: string, args: expression list, line: int}


datatype function =
     FUNCTION of {id: string, params: varDecl list, returnType: miniType,
                  decls: varDecl list, body: statement list, line: int}


datatype program =
     PROGRAM of {types: typeDecl list, decls: varDecl list, funcs: function list}

end
