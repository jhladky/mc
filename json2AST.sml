(* All of the sml-json callbacks have to return data of the same type.
 * So `carrier` acts as a sort-of 'superclass' for the various parts
 * of the AST.*)
datatype carrier =
     carrier_list of carrier list
   | s_c_pair of string * carrier
   | varDecl of varDecl
   | typeDecl of typeDecl
   | lvalue of lvalue
   | expression of expression
   | statement of statement
   | function of function
   | program of program
   | string of string
   | int of int
   | bool of bool
;

structure Json2Ast : JSON_CALLBACKS = struct

type json_data = carrier

(* HELPER FUNCTIONS *)

fun carrier2Ht L =
    let
        val ht = HashTable.mkTable (HashString.hashString, op =)
                                   (10, Fail "Not Found AST");
        fun carrier2Ht_ (s_c_pair p) = HashTable.insert ht p
          | carrier2Ht_ _ = raise Fail "Expected an `s_c_pair`."
        ;
    in
        (app carrier2Ht_ L;
         ht)
    end
;

fun str2BinaryOpr "+" = BOP_PLUS
  | str2BinaryOpr "-" = BOP_MINUS
  | str2BinaryOpr "*" = BOP_TIMES
  | str2BinaryOpr "/" = BOP_DIVIDE
  | str2BinaryOpr "%" = BOP_MOD
  | str2BinaryOpr "==" = BOP_EQ
  | str2BinaryOpr "!=" = BOP_NE
  | str2BinaryOpr "<" = BOP_LT
  | str2BinaryOpr ">" = BOP_GT
  | str2BinaryOpr "<=" = BOP_LE
  | str2BinaryOpr ">=" = BOP_GE
  | str2BinaryOpr "&&" = BOP_AND
  | str2BinaryOpr "||" = BOP_OR
  | str2BinaryOpr s = raise Fail s
;

fun str2UnaryOpr "!" = UOP_NOT
  | str2UnaryOpr "-" = UOP_MINUS
  | str2UnaryOpr s = raise Fail s
;

fun carrier2MiniType (string "int") = MT_INT
  | carrier2MiniType (string "bool") = MT_BOOL
  | carrier2MiniType (string "void") = MT_VOID
  | carrier2MiniType (string s) = MT_STRUCT s
  | carrier2MiniType _ = raise Fail "Expected a `string`."
;

(*'uwr' stands for 'unwrap'*)
fun uwrStr (string s) = s | uwrStr _ = raise Fail "Expected a `string`.";

fun uwrExpr (expression e) = e
  (* | uwrExpr (statement _) = raise Fail "Expected an `expression`, got statement." *)
  (* | uwrExpr (lvalue _) = raise Fail "Expected an `expression`, got lvalue." *)
  (* | uwrExpr (function _) = raise Fail "Expected an `expression`, got function." *)
  | uwrExpr _ = raise Fail "Expected an `expression`."
;

fun uwrStmt (statement s) = s
  | uwrStmt _ = raise Fail "Expected a `statement`."
;

fun uwrLvalue (lvalue l) = l
  | uwrLvalue _ = raise Fail "Expected a `lvalue`."
;

fun uwrBool (bool b) = b | uwrBool _ = raise Fail "Expected a `bool`.";

fun uwrCL f (carrier_list L) = map f L
  | uwrCL _ _ = raise Fail "Expected a `carrier_list`"
;

fun uwrVds c = uwrCL (fn (varDecl vd) => vd | _ => raise Fail "") c;
fun uwrExprs c = uwrCL uwrExpr c;
fun uwrStmts c = uwrCL uwrStmt c;

fun line ht = (fn (int n) => n | _ => raise Fail "Expected an `int`.")
                  (HashTable.lookup ht "line");

fun expression2Ast ht =
    expression (
        case uwrStr (HashTable.lookup ht "exp") of
            "num" =>
            (case Int.fromString (uwrStr (HashTable.lookup ht "value")) of
                 SOME n => EXP_NUM {value=n, line=line ht}
               | NONE => raise Fail "Bad integer conversion.")
          | "id" =>
            EXP_ID {id=uwrStr (HashTable.lookup ht "id"), line=line ht}
          | "true" =>
            EXP_TRUE {line=line ht}
          | "false" =>
            EXP_FALSE {line=line ht}
          | "null" => EXP_UNDEFINED
          | "binary" =>
            EXP_BINARY {
                opr=str2BinaryOpr (uwrStr (HashTable.lookup ht "operator")),
                lft=uwrExpr (HashTable.lookup ht "lft"),
                rht=uwrExpr (HashTable.lookup ht "rht"),
                line=line ht
            }
          | "unary" =>
            EXP_UNARY {
                opr=str2UnaryOpr (uwrStr (HashTable.lookup ht "operator")),
                opnd=uwrExpr (HashTable.lookup ht "operand"),
                line=line ht
            }
          | "dot" =>
            EXP_DOT {
                lft=uwrExpr (HashTable.lookup ht "left"),
                prop=uwrStr (HashTable.lookup ht "id"),
                line=line ht
            }
          | "new" =>
            EXP_NEW {
                id=uwrStr (HashTable.lookup ht "id"),
                line=line ht
            }
          | "invocation" =>
            EXP_INVOCATION {
                id=uwrStr (HashTable.lookup ht "id"),
                args=uwrExprs (HashTable.lookup ht "args"),
                line=line ht
            }
          | s => raise Fail s
    )
;

(*Note there's some crap in here to make the compiler shut up about
 * non-exhaustive matches.*)
fun statement2Ast ht =
    statement (
        case uwrStr (HashTable.lookup ht "stmt") of
            "block" => ST_BLOCK (uwrStmts (HashTable.lookup ht "list"))
          | "assign" =>
            ST_ASSIGN {
                target=uwrLvalue (HashTable.lookup ht "target"),
                source=uwrExpr (HashTable.lookup ht "source"),
                line=line ht
            }
          | "print" =>
            ST_PRINT {
                body=uwrExpr (HashTable.lookup ht "exp"),
                endl=uwrBool (HashTable.lookup ht "endl"),
                line=line ht
            }
          | "read" =>
            ST_READ {
                id=uwrLvalue (HashTable.lookup ht "target"),
                line=line ht
            }
          | "if" =>
            ST_IF {
                guard=uwrExpr (HashTable.lookup ht "guard"),
                thenBlk=uwrStmt (HashTable.lookup ht "then"),
                elseBlk=(case HashTable.find ht "else" of
                             NONE => ST_BLOCK []
                           | SOME (statement s) => s
                           | SOME _ => raise Fail "Expected a `statament`."),
                line=line ht
            }
          | "while" =>
            ST_WHILE {
                guard=uwrExpr (HashTable.lookup ht "guard"),
                body=uwrStmt (HashTable.lookup ht "body"),
                line=line ht
            }
          | "delete" =>
            ST_DELETE {
                exp=uwrExpr (HashTable.lookup ht "guard"),
                line=line ht
            }
          | "return" =>
            ST_RETURN {
                exp=case HashTable.find ht "exp" of
                        NONE => EXP_UNDEFINED
                      | SOME (expression e) => e
                      | SOME _ => raise Fail "Expected an `expression`.",
                line=line ht
            }
          | "invocation" =>
            ST_INVOCATION {
                id=uwrStr (HashTable.lookup ht "id"),
                args=uwrExprs (HashTable.lookup ht "args"),
                line=line ht
            }
          | s => raise Fail s
    )
;

fun varDecl2Ast ht =
    varDecl (
        VAR_DECL {
            typ=carrier2MiniType (HashTable.lookup ht "type"),
            id=uwrStr (HashTable.lookup ht "id"),
            line=line ht
        }
    )
;

fun typeDecl2Ast ht =
    typeDecl (
        TYPE_DECL {
            id=uwrStr (HashTable.lookup ht "id"),
            decls=uwrVds (HashTable.lookup ht "fields"),
            line=line ht
        }
    )
;

fun function2Ast ht =
    function (
        FUNCTION {
            id=uwrStr (HashTable.lookup ht "id"),
            returnType=carrier2MiniType (HashTable.lookup ht "return_type"),
            params=uwrVds (HashTable.lookup ht "parameters"),
            decls=uwrVds (HashTable.lookup ht "declarations"),
            body=uwrStmts (HashTable.lookup ht "body"),
            line=line ht
        }
    )
;

fun lvalue2Ast ht =
    lvalue (
        case uwrStr (HashTable.lookup ht "lval") of
            "id" =>
            LV_ID {
                id=uwrStr (HashTable.lookup ht "id"),
                line=line ht
            }
          | "dot" =>
            LV_DOT {
                lft=uwrLvalue (HashTable.lookup ht "left"),
                prop=uwrStr (HashTable.lookup ht "id"),
                line=line ht
            }
          | s => raise Fail s
    )
;

fun program2Ast ht =
    program (
        PROGRAM {
            types=uwrCL (fn (typeDecl td) => td | _ => raise Fail "")
                        (HashTable.lookup ht "types"),
            decls=uwrVds (HashTable.lookup ht "declarations"),
            funcs=uwrCL (fn (function f) => f | _ => raise Fail "")
                        (HashTable.lookup ht "functions")
        }
    )
;

fun json_object L =
    let
        val ht = carrier2Ht L;
    in
        case uwrStr (HashTable.lookup ht "ast_node") of
            "expression" => expression2Ast ht
          | "statement" => statement2Ast ht
          | "varDecl" => varDecl2Ast ht
          | "typeDecl" => typeDecl2Ast ht
          | "function" => function2Ast ht
          | "lvalue" => lvalue2Ast ht
          | "program" => program2Ast ht
          | s => raise Fail s
    end
;

fun json_pair p = s_c_pair p;
fun json_array L = carrier_list L;
fun json_value v = v;
fun json_int i = int i;
fun json_bool b = bool b;
fun json_string s = string s;
fun json_real _ = raise Fail "Unexpected `real`.";
fun json_null () = raise Fail "Unexpected `null`.";

fun error_handle (msg, pos, data) =
    raise Fail ("Error: " ^ msg ^ " near " ^ Int.toString pos ^ " data: " ^
                data)
;
end;

structure parser = JSONParser (Json2Ast);

fun json2AST ins =
    case parser.parse (TextIO.inputAll ins) of
        program p => p
      | _ => raise Fail ""
;
            

