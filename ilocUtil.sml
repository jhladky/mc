structure IlocUtil = struct

datatype iloc_info =
         II of {
             id: string,
             st: SymbolTable.symbol_table,
             cfg: Iloc.basic_block Cfg.cfg,
             regs: (string, int) HashTable.hash_table,
             nextReg: int ref,
             mochiCompat: bool
         }


local
    val next = ref 0
in
    fun nextLabel () =
        "L" ^ (Int.toString (!next)) before next := 1 + (!next)
end


fun assignRegs ht =
    let
        val n = ref 0;
    in
        HashTable.map (fn _ => !n before n := 1 + (!n)) ht
    end


fun nextReg (II {nextReg=nextReg, ...}) =
    !nextReg before nextReg := 1 + (!nextReg)


fun mkIi st cfg (Ast.FUNCTION {params=params, decls=decls, id=id, ...}) compat =
    let
        val ht = Util.mkHt ()
        val addVD = (fn (Ast.VAR_DECL {id=s, typ=t, ...}) =>
                        HashTable.insert ht (s, t))
    in
        List.app addVD decls;
        List.app addVD params;
        II {
            id=id,
            st=st,
            cfg=cfg,
            regs=assignRegs ht,
            nextReg=ref (HashTable.numItems ht),
            mochiCompat=compat
        }
    end


fun mkIf cfg node =
    let
        val exitNode = Cfg.mkNode cfg (nextLabel (), [])
        val thenNode = Cfg.mkNode cfg (nextLabel (), [])
        val elseNode = Cfg.mkNode cfg (nextLabel (), [])
    in
        Cfg.addEdge node elseNode;
        Cfg.addEdge node thenNode;
        (thenNode, elseNode, exitNode)
    end


fun mkWhile cfg node =
    let
        val guard = Cfg.mkNode cfg (nextLabel (), [])
        val body = Cfg.mkNode cfg (nextLabel (), [])
        val exit = Cfg.mkNode cfg (nextLabel (), [])
    in
        Cfg.addEdge node guard;
        Cfg.addEdge guard body;
        Cfg.addEdge guard exit;
        (guard, body, exit)
    end


local
    open Cfg
    open Iloc
in
    fun mkReturn cfg = (getExit cfg, mkNode cfg (nextLabel (), []))
    fun getLabel (n: (string * instruction list) node) = #1 (getData n)
end


fun fill node L =
    let
        val (label, oldL) = Cfg.getData node
    in
        Cfg.update node (label, oldL @ L)
    end


local
    fun addOffsets ht n [] = ht
      | addOffsets ht n ((Ast.VAR_DECL {id=id, ...})::xs) =
        (HashTable.insert ht (id, n); addOffsets ht (n + 1) xs)
in
    fun calcOffsets offsets (Ast.TYPE_DECL {id=id, decls=decls, ...}) =
        HashTable.insert offsets (id, addOffsets (Util.mkHt ()) 0 decls)
end


val getVDId = fn (Ast.VAR_DECL {id=s, ...}) => s


fun addType types (Ast.TYPE_DECL {id=id, decls=decls, ...}) =
    HashTable.insert types (id, List.map getVDId decls)

end
