signature COPY_PROP = sig
    val copyProp : Iloc.program -> Iloc.program
end

structure CopyProp :> COPY_PROP = struct
open Iloc



fun optFunc (id, bb) =
    let
        val copies = Util.mkHt ()
    in
        (*need some function for determining copies*)
        (id, bb)
    end

fun copyProp prog = List.map optFunc prog


end
