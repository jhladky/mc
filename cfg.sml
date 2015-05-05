signature CFG = sig
    type node
    type cfg

    val mkCfg : SymbolTable.symbol_table -> Ast.function -> node * node * cfg
    val mkIf : node -> node * node * node
    val mkWhile : node -> node * node * node
    val mkReturn : cfg -> node * node

    val link : node -> node -> unit
    val fill : node -> Iloc.instruction list -> unit
    val toList : cfg -> Iloc.basic_block list
    val nextReg : cfg -> int

    val getRegs : cfg -> (string, int) HashTable.hash_table
    val getLabel : node -> string
    val getSTInfo : cfg -> string * SymbolTable.symbol_table
end

structure Cfg :> CFG = struct
open Iloc

datatype node = NODE of {next: node list ref, data: basic_block ref}

datatype cfg =
    CFG of {
        st: SymbolTable.symbol_table,
        regs: (string, int) HashTable.hash_table,
        nextReg: int ref,
        entry: node,
        exit: node
    }

val nextLabel = ref 0;


fun mkNode () =
    let
        val label = "L" ^ (Int.toString (!nextLabel));
    in
        nextLabel := 1 + (!nextLabel);
        NODE {next=ref [], data=ref (label, [])}
    end


fun assignRegs ht =
    let
        val n = ref 0;
    in
        HashTable.map (fn _ => !n before n := 1 + (!n)) ht
    end


fun mkCfg st (Ast.FUNCTION {params=params, decls=decls, id=id, ...}) =
    let
        val ht = Util.mkHt ();
        val addVD = (fn (Ast.VAR_DECL {id=s, typ=t, ...}) =>
                        HashTable.insert ht (s, t));
        val entry = NODE {next=ref [], data=ref (id, [])}
        val exit = mkNode ();
    in
        app addVD decls;
        app addVD params;
        (entry,
         exit,
         CFG {
             st=st,
             regs=assignRegs ht,
             nextReg=ref (HashTable.numItems ht),
             entry=entry,
             exit=exit
        })
    end


fun nextReg (CFG {nextReg=nextReg, ...}) =
    !nextReg before nextReg := 1 + (!nextReg)


fun getLabel (NODE {data=data, ...}) = #1 (!data)


fun getRegs (CFG {regs=regs, ...}) = regs


fun getSTInfo (CFG {st=st, entry=NODE {data=data, ...}, ...}) = (#1 (!data), st)


(*mmmm mutation... delicious*)
fun link nod1 nod2 =
    let
        val (NODE {next=next, ...}) = nod1;
    in
        next := nod2::(!next)
    end


fun fill (NODE {data=data, ...}) L = data := (#1 (!data), (#2 (!data)) @ L)


fun mkIf node =
    let
        val exitNode = mkNode ();
        val thenNode = mkNode ();
        val elseNode = mkNode ();
    in
        link node elseNode;
        link node thenNode;
        (thenNode, elseNode, exitNode)
    end

fun mkWhile node =
    let
        val guard = mkNode ();
        val body = mkNode ();
        val exit = mkNode ();
    in
        link node guard;
        link guard body;
        link guard exit;
        (guard, body, exit)
    end


fun mkReturn (CFG {exit=exit, ...}) =
    let
        val node = mkNode ();
    in
        (exit, node)
    end


fun toList1 (nod as NODE {next=next, data=data}, L) =
    if not (isSome (List.find (fn item => item = !data) L)) then
        foldr toList1 (L @ [!data]) (!next)
    else L


fun toList (CFG {entry=en as NODE {data=dEn, ...},
                 exit=NODE {data=dEx, ...}, ...}) =
    let
        val L = List.filter (fn p => p <> !dEn andalso p <> !dEx)
                            (toList1 (en, []))
    in
        [!dEn] @ L @ [!dEx]
    end

end
