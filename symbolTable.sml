signature SYMBOL_TABLE = sig
datatype symbolTable =
    ST of {
        types: (string, (string, Ast.typ) HashTable.hash_table)
                    HashTable.hash_table,
        globals: (string, Ast.typ) HashTable.hash_table,
        funcs: (string, Ast.function) HashTable.hash_table,
        locals: (string, (string, Ast.typ) HashTable.hash_table)
                     HashTable.hash_table
   }


datatype symbolTableLocal =
    STL of {
        types: (string, (string, Ast.typ) HashTable.hash_table)
                   HashTable.hash_table,
        globals: (string, Ast.typ) HashTable.hash_table,
        locals: (string, Ast.typ) HashTable.hash_table,
        funcs: (string, Ast.function) HashTable.hash_table,
        returnType: Ast.typ
    }

val mkSymbolTable : string -> Ast.program -> symbolTable

end

structure SymbolTable :> SYMBOL_TABLE = struct
open HashTable
open Ast

exception BadTypeException of int * typ


(* Variables and functions are in the same namespace,
 * but structure names are in a DIFFERENT namespace.*)
datatype symbolTable =
    ST of {
        types: (string, (string, Ast.typ) hash_table) hash_table,
        globals: (string, Ast.typ) hash_table,
        funcs: (string, Ast.function) hash_table,
        locals: (string, (string, Ast.typ) hash_table) hash_table
   }


datatype symbolTableLocal =
    STL of {
        types: (string, (string, typ) hash_table) hash_table,
        globals: (string, typ) hash_table,
        locals: (string, typ) hash_table,
        funcs: (string, function) hash_table,
        returnType: typ
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
        List.app (fn func as FUNCTION {id=id, ...} =>
                     insert funcs (id, func)) fs;
        List.app (addLocals types locals globals) fs;
        ST {types=types, globals=globals, funcs=funcs, locals=locals}
    end
end
