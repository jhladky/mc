(* All of the sml-json callbacks have to return data of the same type.
 * So `carrier` acts as a sort-of 'superclass' for the various parts
 * of the AST.*)
datatype carrier =
     carrier_list of carrier list
   | s_c_pair of string * carrier
   | varDecl of Ast.varDecl
   | typeDecl of Ast.typeDecl
   | lvalue of Ast.lvalue
   | expression of Ast.expression
   | statement of Ast.statement
   | function of Ast.function
   | program of Ast.program
   | string of string
   | int of int
   | bool of bool


(*Note there's some crap in here to make the compiler shut up about
 * non-exhaustive matches.*)
structure Json2Ast : JSON_CALLBACKS = struct
open Ast;

type json_data = carrier

(* HELPER FUNCTIONS *)
fun carrier2Ht L =
    let
        val ht = HashTable.mkTable (HashString.hashString, op =)
                                   (10, Fail "Not Found");
        fun carrier2Ht_ (s_c_pair p) = HashTable.insert ht p
          | carrier2Ht_ _ = raise Fail "Expected an `s_c_pair`."
    in
        (app carrier2Ht_ L; ht)
    end


fun str2BinaryOpr "+" = BOP_PLUS
  | str2BinaryOpr "-" = BOP_MINUS
  | str2BinaryOpr "*" = BOP_TIMES
  | str2BinaryOpr "/" = BOP_DIVIDE
  | str2BinaryOpr "==" = BOP_EQ
  | str2BinaryOpr "!=" = BOP_NE
  | str2BinaryOpr "<" = BOP_LT
  | str2BinaryOpr ">" = BOP_GT
  | str2BinaryOpr "<=" = BOP_LE
  | str2BinaryOpr ">=" = BOP_GE
  | str2BinaryOpr "&&" = BOP_AND
  | str2BinaryOpr "||" = BOP_OR
  | str2BinaryOpr s = raise Fail s


fun str2UnaryOpr "!" = UOP_NOT
  | str2UnaryOpr "-" = UOP_MINUS
  | str2UnaryOpr s = raise Fail s


fun carrier2MiniType (string "int") = MT_INT
  | carrier2MiniType (string "bool") = MT_BOOL
  | carrier2MiniType (string "void") = MT_VOID
  | carrier2MiniType (string s) = MT_STRUCT s
  | carrier2MiniType _ = raise Fail "Expected a `string`."


(*'uwr' stands for 'unwrap'*)
fun uwrStr (string s) = s | uwrStr _ = raise Fail "Expected a `string`."


fun uwrExpr (expression e) = e
  | uwrExpr _ = raise Fail "Expected an `expression`."


fun uwrStmt (statement s) = s
  | uwrStmt _ = raise Fail "Expected a `statement`."


fun uwrLvalue (lvalue l) = l
  | uwrLvalue _ = raise Fail "Expected a `lvalue`."


fun uwrBool (bool b) = b | uwrBool _ = raise Fail "Expected a `bool`."


fun uwrCL f (carrier_list L) = map f L
  | uwrCL _ _ = raise Fail "Expected a `carrier_list`"


fun uwrVds c = uwrCL (fn (varDecl vd) => vd | _ => raise Fail "") c


fun uwrExprs c = uwrCL uwrExpr c


fun uwrStmts c = uwrCL uwrStmt c


fun line get = (fn (int n) => n | _ => raise Fail "Expected an `int`.")
                   (get "line")


fun expression2Ast get =
    case uwrStr (get "exp") of
        "num" =>
        (case Int.fromString (uwrStr (get "value")) of
             SOME n => EXP_NUM {value=n, line=line get}
           | NONE => raise Fail "Bad integer conversion.")
      | "id" => EXP_ID {id=uwrStr (get "id"), line=line get}
      | "true" => EXP_TRUE {line=line get}
      | "false" => EXP_FALSE {line=line get}
      | "null" => EXP_NULL
      | "binary" =>
        EXP_BINARY {
            opr=str2BinaryOpr (uwrStr (get "operator")),
            lft=uwrExpr (get "lft"),
            rht=uwrExpr (get "rht"),
            line=line get
        }
      | "unary" =>
        EXP_UNARY {
            opr=str2UnaryOpr (uwrStr (get "operator")),
            opnd=uwrExpr (get "operand"),
            line=line get
        }
      | "dot" =>
        EXP_DOT {
            lft=uwrExpr (get "left"),
            prop=uwrStr (get "id"),
            line=line get
        }
      | "new" => EXP_NEW {id=uwrStr (get "id"), line=line get}
      | "invocation" =>
        EXP_INVOCATION {
            id=uwrStr (get "id"),
            args=uwrExprs (get "args"),
            line=line get
        }
      | s => raise Fail s


fun statement2Ast get =
    case uwrStr (get "stmt") of
        "block" => ST_BLOCK (uwrStmts (get "list"))
      | "assign" =>
        ST_ASSIGN {
            target=uwrLvalue (get "target"),
            source=uwrExpr (get "source"),
            line=line get
        }
      | "print" =>
        ST_PRINT {
            body=uwrExpr (get "exp"),
            endl=uwrBool (get "endl"),
            line=line get
        }
      | "read" => ST_READ {id=uwrLvalue (get "target"), line=line get}
      | "if" =>
        ST_IF {
            guard=uwrExpr (get "guard"),
            thenBlk=uwrStmt (get "then"),
            elseBlk=(uwrStmt (get "else") handle Fail _ => ST_BLOCK []),
            line=line get
        }
      | "while" =>
        ST_WHILE {
            guard=uwrExpr (get "guard"),
            body=uwrStmt (get "body"),
            line=line get
        }
      | "delete" => ST_DELETE {exp=uwrExpr (get "guard"), line=line get}
      | "return" =>
        ST_RETURN {
            exp=(SOME (uwrExpr (get "exp")) handle Fail _ => NONE),
            line=line get
        }
      | "invocation" =>
        ST_INVOCATION {
            id=uwrStr (get "id"),
            args=uwrExprs (get "args"),
            line=line get
        }
      | s => raise Fail s


fun varDecl2Ast get =
    VAR_DECL {
        typ=carrier2MiniType (get "type"),
        id=uwrStr (get "id"),
        line=line get
    }


fun typeDecl2Ast get =
    TYPE_DECL {
        id=uwrStr (get "id"),
        decls=uwrVds (get "fields"),
        line=line get
    }


fun function2Ast get =
    FUNCTION {
        id=uwrStr (get "id"),
        returnType=carrier2MiniType (get "return_type"),
        params=uwrVds (get "parameters"),
        decls=uwrVds (get "declarations"),
        body=uwrStmts (get "body"),
        line=line get
    }


fun lvalue2Ast get =
    case uwrStr (get "lval") of
        "id" => LV_ID {id=uwrStr (get "id"), line=line get}
      | "dot" =>
        LV_DOT {
            lft=uwrLvalue (get "left"),
            prop=uwrStr (get "id"),
            line=line get
        }
      | s => raise Fail s


fun program2Ast get =
    PROGRAM {
        types=uwrCL (fn (typeDecl td) => td | _ => raise Fail "")
                    (get "types"),
        decls=uwrVds (get "declarations"),
        funcs=uwrCL (fn (function f) => f | _ => raise Fail "")
                    (get "functions")
    }


fun json_object L =
    let
        val ht = carrier2Ht L;
    in
        case uwrStr (HashTable.lookup ht "ast_node") of
            "expression" => expression (expression2Ast (HashTable.lookup ht))
          | "statement" => statement (statement2Ast (HashTable.lookup ht))
          | "varDecl" => varDecl (varDecl2Ast (HashTable.lookup ht))
          | "typeDecl" => typeDecl (typeDecl2Ast (HashTable.lookup ht))
          | "function" => function (function2Ast (HashTable.lookup ht))
          | "lvalue" => lvalue (lvalue2Ast (HashTable.lookup ht))
          | "program" => program (program2Ast (HashTable.lookup ht))
          | s => raise Fail s
    end


fun json_pair p = s_c_pair p;
fun json_array L = carrier_list L;
fun json_value v = v;
fun json_int i = int i;
fun json_bool b = bool b;
fun json_string s = string s;
fun json_real _ = raise Fail "Unexpected `real`.";
fun json_null () = raise Fail "Unexpected `null`.";


fun error_handle (msg, pos, data) =
    raise Fail ("Error: " ^ msg ^ " near " ^ Int.toString pos)

end

structure parser = JSONParser (Json2Ast);

fun json2AST ins =
    case parser.parse (TextIO.inputAll ins) of
        program p => p
      | _ => raise Fail ""
