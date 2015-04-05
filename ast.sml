datatype binaryOperator =
     BOP_PLUS
   | BOP_MINUS
   | BOP_TIMES
   | BOP_DIVIDE
   | BOP_MOD
   | BOP_EQ
   | BOP_NE
   | BOP_LT
   | BOP_GT
   | BOP_LE
   | BOP_GE
   | BOP_AND
   | BOP_OR
;

datatype unaryOperator =
     UOP_NOT
   | UOP_MINUS
;

datatype miniType =
    MT_VOID
  | MT_INT
  | MT_BOOL
  | MT_STRUCT of string
;

datatype varDecl =
    VAR_DECL of (miniType * string)
;

datatype typeDecl =
    TYPE_DECL of {id: string, decls: varDecl list}
;

datatype lvalue =
    LVALUE of string
;

datatype expression =
     EXP_NUM of int
   | EXP_ID of string
   | EXP_TRUE
   | EXP_FALSE
   | EXP_UNDEFINED
   | EXP_BINARY of {opr: binaryOperator, lft: expression, rht: expression}
   | EXP_UNARY of {opr: unaryOperator, opnd: expression}
   | EXP_DOT of {lft: expression, prop: lvalue}
   | EXP_NEW of string
   | EXP_INVOCATION of {id: string, args: expression list}
;

datatype statement =
     ST_BLOCK of statement list
   | ST_ASSIGN of {target: lvalue, source: expression}
   | ST_PRINT of {body: expression, endl: bool}
   | ST_READ of lvalue
   | ST_IF of {guard: expression, thenBlk: statement, elseBlk: statement}
   | ST_WHILE of {guard: expression, body: statement}
   | ST_DELETE of expression
   | ST_RETURN of expression (* expression can be undefined*)
   | ST_INVOCATION of {id: string, args: expression list}
;

datatype function =
     FUNCTION of {id: string, params: varDecl list, returnType: miniType, decls: varDecl list, body: statement list}
;

datatype program =
     PROGRAM of {types: typeDecl list, decls: varDecl list, funcs: function list}
;
