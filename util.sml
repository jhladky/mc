signature UTIL = sig
    val mkHt : unit -> (string, 'a) HashTable.hash_table
    val WORD_SIZE : int;
end

structure Util :> UTIL = struct

fun mkHt () = HashTable.mkTable (HashString.hashString, op =)
                                (10, Fail "Not Found")

val WORD_SIZE = 8;

end
