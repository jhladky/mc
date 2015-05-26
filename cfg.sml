signature CFG = sig
    type 'a node
    type 'a cfg

    val mkCfg : 'a -> 'a -> 'a node * 'a node * 'a cfg

    val toList : 'a cfg -> 'a list
    val mkNode : 'a cfg -> 'a -> 'a node
    val addEdge : 'a node -> 'a node -> unit

    val map : ('a -> 'b) -> 'a cfg -> 'b cfg
    val fold : ('a * 'b -> 'b) -> 'b -> 'a cfg -> 'b

    val update : 'a node -> 'a -> unit
    val app : ('a node -> 'a) -> 'a cfg -> unit
    val getData : 'a node -> 'a
    val getSuccs : 'a node -> 'a list
    val getPreds : 'a node -> 'a list

    val getExit : 'a cfg -> 'a node (* Remove later *)
end

structure Cfg :> CFG = struct

datatype 'a node =
         NODE of {
             id: int ref,
             next: 'a node list ref,
             prev: 'a node list ref,
             data: 'a ref
         }

datatype 'a cfg =
         CFG of {
             nodes: 'a node list ref,
             entry: 'a node,
             exit: 'a node
         }


fun mkNode (CFG {nodes=nodes, ...}) data =
    let
        val node = NODE {id=ref 0, next=ref [], prev=ref [], data=ref data}
    in
        nodes := node::(!nodes);
        node
    end


fun mkCfg enData exData =
    let
        val entry = NODE {id=ref 0, next=ref [], prev=ref [], data=ref enData}
        val exit = NODE {id=ref 0, next=ref [], prev=ref [], data=ref exData}
    in
        (entry, exit, CFG {nodes=ref [entry, exit], entry=entry, exit=exit})
    end


fun addEdge (node1 as NODE {next=next, ...}) (node2 as NODE {prev=prev, ...}) =
    (next := node2::(!next); prev := node1::(!prev))


(*mmmm mutation... delicious*)
fun update (NODE {data=data, ...}) newData = data := newData
fun getExit (CFG {exit=exit, ...}) = exit
fun getData (NODE {data=data, ...}) = !data
fun find nId nodes = List.find (fn NODE {id=id, ...} => id = nId) nodes
fun getSuccs (NODE {next=next, ...}) = List.map getData (!next)
fun getPreds (NODE {prev=prev, ...}) = List.map getData (!prev)


fun toRep f (CFG {nodes=nodes, entry=en as NODE {id=enId, ...},
                  exit=ex as NODE {id=exId, ...}}) =
    let
        val L = List.filter (fn NODE {id=id, ...} => id <> enId andalso
                                                     id <> exId) (!nodes)
    in
        List.map f ([en] @ L @ [ex])
    end


fun toList cfg = toRep getData cfg
fun fold f init cfg = foldl f init (toList cfg)


fun map1 f L (NODE {id=id, data=data, next=next, prev=prev}) =
    case find id (!L) of
        SOME newNode => newNode
      | NONE =>
        let
            val newNext = ref []
            val newPrev = ref []
            val newNode = NODE {id=id, data=ref (f (!data)),
                                next=newNext, prev=newPrev}
        in
            L := newNode::(!L);
            newNext := List.map (map1 f L) (!next);
            newPrev := List.map (map1 f L) (!prev);
            newNode
        end


fun map f (CFG {entry=entry, exit=NODE {id=id, ...}, ...}) =
    let
        val nodes = ref []
        val newEntry = map1 f nodes entry
    in
        CFG {entry=newEntry, exit=valOf (find id (!nodes)), nodes=nodes}
    end


fun app f (CFG {nodes=nodes, ...}) =
    List.app (fn n as NODE {data=data, ...} => data := f n) (!nodes)

end
