signature STRIP_DEAD_CODE = sig
    val stripDeadCode : Iloc.program -> Iloc.program
end

structure StripDeadCode :> STRIP_DEAD_CODE = struct
open Iloc
open UnorderedSet

type definition = int * string * int

datatype live_variable_analysis =
         LVA of {
             id: string,
             ins: instruction list,
             gk: definition set * definition set,
             reaches: definition set,
             diff: bool
         }


fun insToGK (gen, kill) ins = (gen, kill)


fun bbToGK (id, ins) =
    LVA {
        id=id,
        ins=ins,
        gk=List.foldr (fn (ins, gk) => insToGK gk ins)
                      (empty (), empty ()) ins,
        reaches=empty (),
        diff=true
    }


fun diffCheck (LVA {diff=d, ...}, diff) = if d then true else diff


fun bbReaches node =
    let
        val LVA {id=id, ins=ins, gk=gk, reaches=reaches, ...} = Cfg.getData node
    in
        (* fixme *)
        LVA {id=id, ins=ins, gk=gk, reaches=reaches, diff=false}
    end


fun buildLvas cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app bbReaches cfg; buildLvas cfg)
    else cfg


fun optFunc (id, cfg) =
    let
        val lvas = buildLvas (Cfg.map bbToGK cfg)
    in
        (id, cfg)
    end


fun stripDeadCode prog = List.map optFunc prog

end
