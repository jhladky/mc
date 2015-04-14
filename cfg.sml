signature CFG = sig
    type node;
    type cfg;

    val mkCfg : node -> node -> function -> cfg;
    val mkNode : unit -> node;
    val link : node -> node -> unit;
    val fill : node -> instruction list -> unit;
    val toList : cfg -> basicBlock list;

    (*Get rid of these later*)
    val getExit : cfg -> node;
    val getLocals : cfg -> (string, miniType) HashTable.hash_table;
end

structure Cfg :> CFG = struct

datatype node =
     NODE of {prev: node list ref, next: node list ref,
              bb: instruction list ref, label: string}

datatype cfg =
     CFG of {locals: (string, miniType) HashTable.hash_table,
             entry: node, exit: node}

val nextLabel = ref 0;

fun mkNode () =
    let
        val label = "L" ^ (Int.toString (!nextLabel));
    in
        nextLabel := 1 + (!nextLabel);
        NODE {prev=ref [], next=ref [], bb=ref [], label=label}
    end


fun mkCfg entry exit (FUNCTION {params=params, decls=decls, ...}) =
    let
        val ht = HashTable.mkTable (HashString.hashString, op =)
                                   (10, Fail "Not Found CFG");
        val addVD = (fn (VAR_DECL {id=s, typ=t, ...}) =>
                        HashTable.insert ht (s, t));
    in
        app addVD decls;
        app addVD params;
        CFG {locals=ht, entry=entry, exit=exit}
    end


fun getExit (CFG {exit=exit, ...}) = exit


fun getLocals (CFG {locals=locals, ...}) = locals;


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
    fun toList (CFG {entry=entry, ...}) = toList1 (entry, [])
end

end
