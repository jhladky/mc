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
    T_VOID
  | T_INT
  | T_BOOL
  | T_STRUCT of string
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

(* The comment indicates what the expression is called in the JSON.*)
datatype expression =
     EXP_NUM of int (*num*)
   | EXP_ID of string (*id*)
   | EXP_TRUE (*true*)
   | EXP_FALSE (*false*)
   | EXP_UNDEFINED (*null*)
   | EXP_BINARY of {opr: binaryOperator, lft: expression, rht: expression} (*binary*)
   | EXP_UNARY of {opr: unaryOperator, opnd: expression} (*unary*)
   | EXP_ACCESS of {lft: expression, prop: lvalue} (*dot*)
   | EXP_NEW of string (*new*)
   | EXP_INVOKE of {id: string, args: expression list} (*invovation*)
;

datatype statement =
     ST_BLOCK of statement list (*block*)
   | ST_ASSIGN of {target: lvalue, source: expression} (*assign*)
   | ST_PRINT of {body: expression, endl: bool} (*print*)
   | ST_READ of lvalue (*read*)
   | ST_COND of {guard: expression, thenBlk: statement, elseBlk: statement} (*if*)
   | ST_LOOP of {guard: expression, body: statement} (*while*)
   | ST_DELETE of expression (*delete*)
   | ST_RETURN of expression (* expression can be undefined*) (*return*)
   | ST_INVOKE of {id: string, args: expression list} (*invocation*)
;

datatype function =
     FUNCTION of {id: string, params: varDecl list, returnType: miniType, decls: varDecl list, body: statement list}
;

datatype program =
     PROGRAM of {types: typeDecl list, decls: varDecl list, funcs: function list}
;
