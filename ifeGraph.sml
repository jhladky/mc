signature IFE_GRAPH = sig
    type node
    type ife_graph

    exception NotFound

    val mkGraph : unit -> ife_graph
    val toList : ife_graph -> TargetAmd64.register list
    val mkNode : ife_graph -> TargetAmd64.register -> node
    val find : ife_graph -> TargetAmd64.register -> node option
    val remove : ife_graph -> node -> unit
    (* Not sure about this one. *)
    val filter : ife_graph -> (node -> bool) -> node list

    val addEdge : node -> node -> unit
    val numEdges : node -> int
    val getData : node -> TargetAmd64.register
    val neighbors : node -> TargetAmd64.register list
end

structure IfeGraph :> IFE_GRAPH = struct
open TargetAmd64

exception NotFound

datatype node = NODE of {adj: node list ref, data: register}

datatype ife_graph = IG of {nodes: node list ref}


fun mkNode (IG {nodes=nodes}) data =
    let
        val node = NODE {adj=ref [], data=data}
    in
        nodes := node::(!nodes);
        node
    end


fun find (IG {nodes=nodes}) data =
    List.find (fn NODE {data=d, ...} => d = data) (!nodes)


fun has (IG {nodes=nodes}) node =
    case List.find (fn n => node = n) (!nodes) of
        SOME _ => true
      | NONE => false


fun delEdge toRemove (NODE {adj=adj, ...}) =
    adj := List.filter (fn n => n <> toRemove) (!adj)


fun delEdges (node as NODE {adj=adj, ...}) = List.app (delEdge node) (!adj)


fun filter (IG {nodes=nodes}) f = List.filter f (!nodes)


(* Remove the node from the graph.
 * Raises NotFound if the node does not belong to this graph. *)
fun remove (ig as IG {nodes=nodes}) node =
    if not (has ig node) then raise NotFound
    else (delEdges node; nodes := List.filter (fn n => n <> node) (!nodes))


fun addEdge (node1 as NODE {adj=adj1, ...}) (node2 as NODE {adj=adj2, ...}) =
    (adj1 := node2::(!adj1); adj2 := node1::(!adj2))


fun mkGraph () = IG {nodes=ref []}
fun getData (NODE {data=data, ...}) = data
fun neighbors (NODE {adj=adj, ...}) = List.map getData (!adj)
fun toList (IG {nodes=nodes}) = List.map getData (!nodes)
fun numEdges (NODE {adj=adj, ...}) = length (!adj)

end
