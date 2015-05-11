signature UTIL = sig
    val mkHt : unit -> (string, 'a) HashTable.hash_table
    val fail : string -> int -> string -> unit
    val foldd : string -> ('a -> string) -> 'a list -> string
    val WORD_SIZE : int
end

structure Util :> UTIL = struct

fun mkHt () = HashTable.mkTable (HashString.hashString, op =)
                                (10, Fail "Not Found")


fun fail file l msg =
    (TextIO.output (TextIO.stdErr, file ^ ":" ^ (Int.toString l) ^ ":" ^ msg);
     OS.Process.exit OS.Process.failure)


fun foldd sep f [] = ""
  | foldd sep f (x::[]) = f x
  | foldd sep f (x::xs) = (f x) ^ sep ^ (foldd sep f xs)


val WORD_SIZE = 8;

end
