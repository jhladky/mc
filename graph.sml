(*signature for what the node contents must be*)
signature GRAPH_DATA = sig
    type graph_data
    val equals : graph_data -> graph_data -> bool
end

functor Graph (GraphData : GRAPH_DATA) :
        sig
            type node
            type graph
            type graph_data

            val mkNode : graph_data -> node
            val mkGraph : graph_data -> graph_data -> node * node * graph
            val addEdge : node -> node -> unit
            val getData : node -> graph_data
            val modData : node -> graph_data -> unit
            val toList : graph -> graph_data list
        end
= struct

type graph_data = GraphData.graph_data


datatype node =
         NODE of {
             prev: node list ref,
             next: node list ref,
             data: graph_data ref
         }


datatype graph = GRAPH of {entry: node, exit: node}


fun mkNode data = NODE {prev=ref [], next=ref [], data=ref data}


fun mkGraph entryData exitData =
    let
        val entry = mkNode entryData
        val exit = mkNode exitData
    in
        (entry, exit, GRAPH {entry=entry, exit=exit})
    end


fun addEdge node1 node2 =
    let
        val NODE {next=next, ...} = node1
        val NODE {prev=prev, ...} = node2
    in
        next := node2::(!next);
        prev := node1::(!prev)
    end


fun getData (NODE {data=data, ...}) = !data


fun modData (NODE {data=data, ...}) newData = data := newData


local
    fun eq dRef d = GraphData.equals d (!dRef)

    fun toList1 (nod as NODE {prev=prev, next=next, data=data}, L) =
        if not (isSome (List.find (eq data) L)) then
            foldr toList1 (L @ [!data]) (!next)
        else L
in
    fun toList (GRAPH {entry=en as NODE {data=enD, ...},
                       exit=NODE {data=exD, ...}}) =
        let
            val L = List.filter (fn d => not (eq enD d) andalso not (eq exD d))
                                (toList1 (en, []))
        in
            [!enD] @ L @ [!exD]
        end
end

end
