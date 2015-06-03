(* Implementation of an unordered set using lists.
 * This is not efficient. *)
signature UORD_SET = sig
    type ''a set

    exception NotFound

    val empty : unit -> ''a set
    val singleton : ''a -> ''a set
    val add : ''a set * ''a -> ''a set
    val add' : ''a * ''a set -> ''a set
    val addList : ''a set * ''a list -> ''a set
    val delete : ''a set * ''a -> ''a set
    val member : ''a set * ''a -> bool
    val isEmpty : ''a set -> bool
    val equal : ''a set * ''a set -> bool
    (* val compare : ''a set * ''a set -> order *) (* Not implemented *)
    val isSubset : ''a set * ''a set -> bool
    val numItems : ''a set -> int
    val listItems : ''a set -> ''a list
    val union : ''a set * ''a set -> ''a set
    val intersection : ''a set * ''a set -> ''a set
    val difference : ''a set * ''a set -> ''a set
    val map : (''a -> ''b) -> ''a set -> ''b set
    val app : (''a -> unit) -> ''a set -> unit
    (* TODO: Order doesn't matter in sets. Get rid of one of the fold functions. *)
    val foldl : ((''a * 'b) -> 'b) -> 'b -> ''a set -> 'b
    val foldr : ((''a * 'b) -> 'b) -> 'b -> ''a set -> 'b
    val filter : (''a -> bool) -> ''a set -> ''a set
    val exists : (''a -> bool) -> ''a set -> bool
    val find : (''a -> bool) -> ''a set -> ''a option

    val toString : (''a -> string) -> ''a set -> string
    (* Remove a "random" item from the set, return the item and the new set.*)
    val pick : ''a set -> (''a * ''a set) option
end

(* TODO: Rework to take in an equality function instead of
 * requiring an equality type.*)
structure UnorderedSet :> UORD_SET = struct

exception NotFound

datatype ''a set = SET of ''a list

(* Helper function. O(n) *)
fun has item L = List.exists (fn i => i = item) L


fun addList (SET L, items) =
    SET (foldr (fn (item, L) => if has item L then L else item::L) L items)


fun delete (SET L, item) =
    case List.find (fn i => i = item) L of
        NONE => raise NotFound
      | SOME _ => SET (List.filter (fn i => i <> item) L)


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


fun equal1 other [] = true
  | equal1 other (x::xs) =
    if has x other then equal1 other xs else false


(* Returns true iff each element of one set is present in the other O(n^2). *)
fun equal (SET L1, SET L2) =
    if equal1 L1 L2 andalso equal1 L2 L1 then true else false


(* TODO: Change this to mkSet and add a param for an equality function. *)
fun empty () = SET []
fun singleton item = SET [item]
fun add (s as SET L, item) = if has item L then s else SET (item::L)
fun add' (item, s as SET L) = if has item L then s else SET (item::L)
fun pick (SET L) = if length L = 0 then NONE else SOME (hd L, SET (tl L))
fun member (SET L, item) = has item L
fun isEmpty (SET L) = length L = 0
fun numItems (SET L) = length L
fun listItems (SET L) = L
fun map f (SET L) = SET (List.map f L)
fun app f (SET L) = List.app f L
fun foldl f start (SET L) = List.foldl f start L
fun foldr f start (SET L) = List.foldr f start L
fun exists f (SET L) = List.exists f L
fun filter f (SET L) = SET (List.filter f L)
fun find f (SET L) = List.find f L
fun toString toStr (SET L) = "{" ^ (Util.foldd ", " toStr L) ^ "}"

end
