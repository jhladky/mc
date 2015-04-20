open TargetAmd64;

signature CFG2AMD64 = sig
    val cfg2Amd64 : (string, Cfg.cfg) HashTable.hash_table -> instruction list;
end

structure Cfg2Amd64 :> CFG2AMD64 = struct

fun cfg2Amd64 ht = []

end
