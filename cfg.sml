

signature CFG = sig
    type node;
    type cfg;
    datatype function = FUNCTION of {id: string, body: cfg};
    datatype program =
        PROGRAM of {
            types: Ast.typeDecl list,
            decls: Ast.varDecl list,
            funcs: function list
        };

    val mkCfg : Ast.function -> node * node * cfg;
    val mkIf : node -> node * node * node;
    val mkWhile : node -> node * node * node;
    val mkReturn : cfg -> node * node;

    val link : node -> node -> unit;
    val fill : node -> Iloc.instruction list -> unit;
    val toList : function -> Iloc.basicBlock list;
    val nextReg : cfg -> int;

    (*Get rid of these later*)
    val getRegs : cfg -> (string, int) HashTable.hash_table;
    val getLabel : node -> string;
    val getName : cfg -> string;
end

structure Cfg :> CFG = struct
open Iloc;

datatype node =
    NODE of {
        label: string,
        prev: node list ref,
        next: node list ref,
        bb: instruction list ref
    }

datatype cfg =
    CFG of {
        regs: (string, int) HashTable.hash_table,
        nextReg: int ref,
        entry: node, exit: node
    }

datatype function = FUNCTION of {id: string, body: cfg};

datatype program =
    PROGRAM of {
        types: Ast.typeDecl list,
        decls: Ast.varDecl list,
        funcs: function list
    }


val nextLabel = ref 0;


fun mkNode () =
    let
        val label = "L" ^ (Int.toString (!nextLabel));
    in
        nextLabel := 1 + (!nextLabel);
        NODE {prev=ref [], next=ref [], bb=ref [], label=label}
    end


fun assignRegs ht =
    let
        val n = ref 0;
    in
        HashTable.map (fn _ => !n before n := 1 + (!n)) ht
    end


fun mkCfg (Ast.FUNCTION {params=params, decls=decls, id=id, ...}) =
    let
        val ht = HashTable.mkTable (HashString.hashString, op =)
                                   (10, Fail "Not Found CFG");
        val addVD = (fn (Ast.VAR_DECL {id=s, typ=t, ...}) =>
                        HashTable.insert ht (s, t));
        val entry = NODE {prev=ref [], next=ref [], bb=ref [], label=id};
        val exit = mkNode ();
    in
        app addVD decls;
        app addVD params;
        (entry,
         exit,
         CFG {
             regs=assignRegs ht,
             nextReg=ref (HashTable.numItems ht),
             entry=entry,
             exit=exit
        })
    end


fun nextReg (CFG {nextReg=nextReg, ...}) =
    !nextReg before nextReg := 1 + (!nextReg)


fun getLabel (NODE {label=label, ...}) = label


fun getRegs (CFG {regs=regs, ...}) = regs


fun getName (CFG {entry=NODE {label=label, ...}, ...}) = label


(*mmmm mutation... delicious*)
fun link nod1 nod2 =
    let
        val (NODE {next=next, ...}) = nod1;
        val (NODE {prev=prev, ...}) = nod2;
    in
        next := nod2::(!next);
        prev := nod1::(!prev)
    end


fun fill node L =
    let
        val (NODE {bb=bb, ...}) = node;
    in
        bb := (!bb) @ L
    end


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


local
    fun toList1 (nod as NODE {prev=prev, next=next, bb=bb, label=label}, L) =
        if not (isSome (List.find (fn item => item = (label, !bb)) L)) then
            (* foldr toList1 (foldr toList1 (L @ [(label, !bb)]) (!prev)) (!next) *)
            foldr toList1 (L @ [(label, !bb)]) (!next)
        else L
in
    fun toList (FUNCTION {id=_, body=CFG {entry=en as NODE {label=enL, bb=enBB, ...},
                                          exit=NODE {label=exL, bb=exBB, ...}, ...}}) =
        let
            val enP = (enL, !enBB)
            val exP = (exL, !exBB)
            val L = List.filter (fn p => p <> enP andalso p <> exP)
                                (toList1 (en, []))
        in
            [enP] @ L @ [exP]
        end
end

end
