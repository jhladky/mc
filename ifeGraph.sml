signature IFE_GRAPH = sig
    type node
    type ife_graph

    val mkGraph : unit -> ife_graph
    val toList : ife_graph -> TargetAmd64.register list
    val mkNode : ife_graph -> TargetAmd64.register -> node
    val find : ife_graph -> TargetAmd64.register -> node option

    val addEdge : node -> node -> unit
    val getData : node -> TargetAmd64.register
    val neighbors : node -> TargetAmd64.register list
end

structure IfeGraph :> IFE_GRAPH = struct
open TargetAmd64

datatype node = NODE of {adj: node list ref, data: register}

datatype ife_graph = IG of {nodes: node list ref}


fun mkNode (IG {nodes=nodes}) data =
    let
        val node = NODE {adj=ref [], data=data}
    in
        nodes := node::(!nodes);
        node
    end


fun addEdge (node1 as NODE {adj=adj1, ...}) (node2 as NODE {adj=adj2, ...}) =
    (adj1 := node2::(!adj1); adj2 := node1::(!adj2))


fun find (IG {nodes=nodes}) data =
    List.find (fn NODE {data=d, ...} => d = data) (!nodes)


fun mkGraph () = IG {nodes=ref []}
fun getData (NODE {data=data, ...}) = data
fun neighbors (NODE {adj=adj, ...}) = List.map getData (!adj)
fun toList (IG {nodes=nodes}) = List.map getData (!nodes)

end
