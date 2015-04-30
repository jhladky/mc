signature SYMBOL_TABLE = sig
    datatype func_info =
        FUNC_INFO of {
            params: Ast.typ list,
            returnType: Ast.typ,
            calls: string list
        }


    datatype symbol_table =
        ST of {
            types: (string, (string, Ast.typ) HashTable.hash_table)
                       HashTable.hash_table,
            globals: (string, Ast.typ) HashTable.hash_table,
            funcs: (string, func_info) HashTable.hash_table,
            locals: (string, (string, Ast.typ) HashTable.hash_table)
                        HashTable.hash_table
        }

    val mkSymbolTable : string -> Ast.program -> symbol_table
end

structure SymbolTable :> SYMBOL_TABLE = struct
open HashTable
open Ast

exception BadTypeException of int * typ

datatype func_info =
    FUNC_INFO of {
        (*may need to include Id in here*)
        params: typ list, (*list of the params the function takes*)
        returnType: typ, (*return type of the function*)
        calls: string list (*list of calls the function makes*)
    }


(* Variables and functions are in the same namespace,
 * but structure names are in a DIFFERENT namespace.*)
datatype symbol_table =
    ST of {
        types: (string, (string, Ast.typ) hash_table) hash_table,
        globals: (string, Ast.typ) hash_table,
        funcs: (string, func_info) hash_table,
        locals: (string, (string, Ast.typ) hash_table) hash_table
    }


fun addTypeDecl types (TYPE_DECL {id=id, decls=ds, ...}) =
    let
        val decls = Util.mkHt ();
    in
        List.app (fn (VAR_DECL {id=s, typ=t, ...}) => insert decls (s, t)) ds;
        insert types (id, decls)
    end


fun checkTypeExist types (VAR_DECL {id=id, typ=t, line=l}) =
    case t of
        MT_STRUCT s =>
        (case find types s of
             SOME _ => ()
           | NONE => raise BadTypeException (l, t))
      | _ => ()


fun mkLocals types globals (FUNCTION {params=params, decls=decls, ...}) =
    let
        val ht = copy globals
    in
        List.app (checkTypeExist types) decls;
        List.app (checkTypeExist types) params;
        List.app (fn (VAR_DECL {id=s, typ=t, ...}) => insert ht (s, t)) decls;
        List.app (fn (VAR_DECL {id=s, typ=t, ...}) => insert ht (s, t)) params;
        ht
    end


fun addLocals types locals globals (func as FUNCTION {id=id, ...}) =
    insert locals (id, mkLocals types globals func)


(*Check if the type is valid. If so, add it to the globals table*)
fun addGlobal globals types (vd as VAR_DECL {id=id, typ=t, ...}) =
    (checkTypeExist types vd; insert globals (id, t))


fun getExpCalls L (EXP_BINARY {lft=lft, rht=rht, ...}) =
    getExpCalls (getExpCalls L rht) lft
  | getExpCalls L (EXP_UNARY {opnd=opnd, ...}) = getExpCalls L opnd
  | getExpCalls L (EXP_DOT {lft=lft, ...}) = getExpCalls L lft
  | getExpCalls L (EXP_INVOCATION {id=id, ...}) = id::L
  | getExpCalls L _ = L


fun getStmtCalls L (ST_BLOCK stmts) =
    foldr (fn (s, L) => getStmtCalls L s) L stmts
  | getStmtCalls L (ST_ASSIGN {source=source, ...}) = getExpCalls L source
  | getStmtCalls L (ST_PRINT {body=body, ...}) = getExpCalls L body
  | getStmtCalls L (ST_IF {guard=guard, thenBlk=tBlk, elseBlk=eBlk, ...}) =
    getStmtCalls (getStmtCalls (getExpCalls L guard) eBlk) tBlk
  | getStmtCalls L (ST_WHILE {guard=guard, body=body, ...}) =
    getStmtCalls (getExpCalls L guard) body
  | getStmtCalls L (ST_DELETE {exp=exp, ...}) = getExpCalls L exp
  | getStmtCalls L (ST_RETURN {exp=exp, ...}) =
    (case exp of SOME e => getExpCalls L e | NONE => L)
  | getStmtCalls L (ST_INVOCATION {id=id, ...}) = id::L
  | getStmtCalls L _ = L


fun makeFuncInfo (FUNCTION {params=params, returnType=rt, body=body, ...}) =
    FUNC_INFO {
        returnType=rt,
        params=List.map (fn VAR_DECL {typ=t, ...} => t) params,
        calls=foldr (fn (s, L) => getStmtCalls L s) [] body
    }


fun addFuncInfo ht (func as FUNCTION {id=id, ...}) =
    insert ht (id, makeFuncInfo func)


fun mkSymbolTable file (PROGRAM {types=ts, decls=ds, funcs=fs}) =
    let
        val types = Util.mkHt ();
        val globals = Util.mkHt ();
        val funcs = Util.mkHt ();
        val locals = Util.mkHt ();
    in
        List.app (addTypeDecl types) ts;
        List.app (addGlobal globals types) ds
        handle BadTypeException (ln, t) =>
               Util.fail file ln ("Type " ^ (typeToStr t) ^
                                  " does not exist.\n");
        List.app (fn FUNCTION {id=id, ...} => insert globals (id, MT_FUNC)) fs;
        List.app (addFuncInfo funcs) fs;


        (* List.app (fn func as FUNCTION {id=id, ...} => (*this is the part i'm going to need to modify*) *)
        (*              insert funcs (id, func)) fs; *)
        List.app (addLocals types locals globals) fs;
        ST {types=types, globals=globals, funcs=funcs, locals=locals}
    end
end
