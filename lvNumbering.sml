signature LV_NUMBERING = sig
    val lvNumbering : Iloc.program -> Iloc.program
end

structure LvNumbering :> LV_NUMBERING = struct
open Util


fun optFunc (id, cfg)=
    (id, cfg)


fun lvNumbering prog = List.map optFunc prog

end
