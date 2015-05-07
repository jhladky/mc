(* Implementation of an unordered set using lists.
 * This is not efficient. *)
signature UORD_SET = sig
    type item = TargetAmd64.register
    type set

    val empty : unit -> set
    val singleton : item -> set
    val add : set * item -> set
    val addList : set * item list -> set
    (* val delete : set * item -> set *) (* Not implemented *)
    val member : set * item -> bool
    val isEmpty : set -> bool
    val equal : set * set -> bool
    (* val compare : set * set -> order *) (* Not implemented *)
    val isSubset : set * set -> bool
    val numItems : set -> int
    val listItems : set -> item list
    val union : set * set -> set
    val intersection : set * set -> set
    val difference : set * set -> set
    val map : (item -> item) -> set -> set
    val app : (item -> unit) -> set -> unit
    val foldl : ((item * 'b) -> 'b) -> 'b -> set -> 'b
    val foldr : ((item * 'b) -> 'b) -> 'b -> set -> 'b
    val filter : (item -> bool) -> set -> set
    val exists : (item -> bool) -> set -> bool
    val find : (item -> bool) -> set -> item option
end

structure UnorderedSet :> UORD_SET = struct

type item = TargetAmd64.register

datatype set = SET of item list


(* Helper function. O(n) *)
fun has item L = List.exists (fn i => i = item) L


fun addList (SET L, items) =
    SET (foldr (fn (item, L) => if has item L then L else item::L) L items)


(* Returns true iff the first set is a subset of the second O(n^2). *)
fun isSubset (SET L1, SET L2) =
    foldr (fn (item, b) => if has item L2 then b else false) true L1


fun union (SET L1, SET L2) =
    SET (foldr (fn (item, L) => if not (has item L) then item::L else L) L1 L2)


fun intersection (SET L1, SET L2) = (*double check this*)
    SET (foldr (fn (item, L) => if has item L2 then item::L else L) [] L1)


(* Returns the elements in the first set not in the second set. *)
fun difference (SET L1, SET L2) =
    SET (foldr (fn (item, L) => if has item L2 then L else item::L) [] L1)


fun empty () = SET []
fun singleton item = SET [item]
fun add (s as SET L, item) = if has item L then s else SET (item::L)
fun add' (item, s as SET L) = if has item L then s else SET (item::L)
fun member (SET L, item) = has item L
fun isEmpty (SET L) = length L = 0
fun equal (SET L1, SET L2) = L1 = L2
fun numItems (SET L) = length L
fun listItems (SET L) = L
fun map f (SET L) = SET (List.map f L)
fun app f (SET L) = List.app f L
fun foldl f start (SET L) = List.foldl f start L
fun foldr f start (SET L) = List.foldr f start L
fun exists f (SET L) = List.exists f L
fun filter f (SET L) = SET (List.filter f L)
fun find f (SET L) = List.find f L

end
