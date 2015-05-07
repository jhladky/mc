signature CFG = sig
    type 'a node
    type 'a cfg

    val mkCfg : 'a -> 'a -> 'a node * 'a node * 'a cfg
    val toList : 'a cfg -> 'a list
    val map : ('a -> 'b) -> 'a cfg -> 'b cfg
    val apply : ('a list -> 'a -> 'a) -> 'a cfg -> unit

    val mkNode : 'a -> 'a node
    val link : 'a node -> 'a node -> unit (* Rename to addEdge *)
    val update : 'a node -> 'a -> unit
    val getData : 'a node -> 'a
    val successors : 'a node -> 'a list

    val getExit : 'a cfg -> 'a node (* Remove later *)
end

structure Cfg :> CFG = struct

datatype 'a node = NODE of {id: int ref, next: 'a node list ref, data: 'a ref}

datatype 'a cfg = CFG of {entry: 'a node, exit: 'a node}


fun mkNode data = NODE {id=ref 0, next=ref [], data=ref data}


fun mkCfg enData exData =
    let
        val entry = mkNode enData
        val exit = mkNode exData
    in
        (entry, exit, CFG {entry=entry, exit=exit}) (*this is bad*)
    end


(*mmmm mutation... delicious*)
fun update (NODE {data=data, ...}) newData = data := newData
fun link (NODE {next=next, ...}) node2 = next := node2::(!next)
fun getExit (CFG {exit=exit, ...}) = exit
fun getData (NODE {data=data, ...}) = !data


fun successors (NODE {next=next, ...}) =
    map (fn NODE {data=data, ...} => !data) (!next)


fun toList1 (node as NODE {id=nId, next=next, ...}, L) =
    if not (isSome (List.find (fn NODE {id=id, ...} => id = nId) L)) then
        foldr toList1 (L @ [node]) (!next)
    else L


fun toList (CFG {entry=en as NODE {id=enId, ...},
                 exit=ex as NODE {id=exId, ...}}) =
    let
        val L = List.filter (fn NODE {id=id, ...} =>
                                id <> enId andalso id <> exId)
                            (toList1 (en, []))
    in
        List.map getData ([en] @ L @ [ex])
    end


fun map1 f L (NODE {id=nId, data=data, next=next}) =
    case List.find (fn NODE {id=id, ...} => id = nId) (!L) of
        SOME newNode => newNode
      | NONE =>
        let
            val newNext = ref []
            val newNode = NODE {id=nId, data=ref (f (!data)), next=newNext}
        in
            (*might be a problem area here...*)
            L := newNode::(!L);
            newNext := List.map (map1 f L) (!next);
            newNode
        end


(* This is bad. *)
fun findExit exitId node =
    let
        val L = toList1 (node, [])
    in
        valOf (List.find (fn NODE {id=id, ...} => id = exitId) L)
    end


fun map f (CFG {entry=entry, exit=NODE {id=id, ...}}) =
    let
        val newEntry = map1 f (ref []) entry
    in
        CFG {entry=newEntry, exit=findExit id newEntry}
    end


(* There's a lot of duplication going on here...
 * find a way to fix it in the future. *)
fun apply1 f L (node as NODE {id=nId, data=data, next=next}) =
    case List.find (fn NODE {id=id, ...} => id = nId) (!L) of
        SOME _ => ()
      | NONE =>
        let
            val succs = List.map getData (foldr toList1 [] (!next))
        in
            L := node::(!L); update node (f succs (!data))
        end


fun apply f (CFG {entry=entry, ...}) = apply1 f (ref []) entry


end
