open Iloc;

signature CFG = sig
    type node;
    type cfg;

    val mkCfg : function -> node * node * cfg;
    val mkNode : unit -> node;
    val link : node -> node -> unit;
    val fill : node -> instruction list -> unit;
    val toList : cfg -> basicBlock list;
    val nextReg : cfg -> int;

    (*Get rid of these later*)
    val getExit : cfg -> node;
    val getRegs : cfg -> (string, int) HashTable.hash_table;
    val getLabel : node -> string;
end

structure Cfg :> CFG = struct

datatype node =
     NODE of {prev: node list ref, next: node list ref,
              bb: instruction list ref, label: string}

datatype cfg =
         CFG of {regs: (string, int) HashTable.hash_table, nextReg: int ref,
             entry: node, exit: node}

val nextLabel = ref 0;


fun mkNode () =
    let
        val label = "L" ^ (Int.toString (!nextLabel));
    in
        nextLabel := 1 + (!nextLabel);
        NODE {prev=ref [], next=ref [], bb=ref [], label=label}
    end


fun mkNodeL l = NODE {prev=ref [], next=ref [], bb=ref [], label=l}


fun assignRegs ht =
    let
        val n = ref 0;
    in
        HashTable.map (fn _ => !n before n := 1 + (!n)) ht
    end


fun mkCfg (FUNCTION {params=params, decls=decls, id=id, ...}) =
    let
        val ht = HashTable.mkTable (HashString.hashString, op =)
                                   (10, Fail "Not Found CFG");
        val addVD = (fn (VAR_DECL {id=s, typ=t, ...}) =>
                        HashTable.insert ht (s, t));
        val entry = mkNodeL id;
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


fun getExit (CFG {exit=exit, ...}) = exit


fun getLabel (NODE {label=label, ...}) = label


fun getRegs (CFG {regs=regs, ...}) = regs


(*mmmm mutation... delicious*)
fun link nod1 nod2 =
    let
        val (NODE {next=next, ...}) = nod1;
        val (NODE {prev=prev, ...}) = nod2;
    in
        next := nod2::(!next);
        prev := nod1::(!prev)
    end


fun fill nod L =
    let
        val (NODE {bb=bb, ...}) = nod;
    in
        bb := (!bb) @ L
    end


local
    fun toList1 (nod as NODE {prev=prev, next=next, bb=bb, label=label}, L) =
        if not (isSome (List.find (fn item => item = (label, !bb)) L)) then
            foldr toList1 (foldr toList1 (L @ [(label, !bb)]) (!prev)) (!next)
        else L
in
    fun toList (CFG {entry=en as NODE {label=enL, bb=enBB, ...},
                     exit=NODE {label=exL, bb=exBB, ...}, ...}) =
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
