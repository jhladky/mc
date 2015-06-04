signature DFA = sig

    datatype ('a, 'b) dataflow_analysis =
             DFA of {
                 id: string,
                 ins: 'a list,
                 gk: 'b UnorderedSet.set * 'b UnorderedSet.set,
                 propSet: 'b UnorderedSet.set,
                 diff: bool
             }

    val buildDFAs : (('a, 'b) dataflow_analysis Cfg.node ->
                     ('a, 'b) dataflow_analysis)
                    -> ('a, 'b) dataflow_analysis Cfg.cfg
                    -> ('a, 'b) dataflow_analysis Cfg.cfg

end

structure Dfa :> DFA = struct

datatype ('a, 'b) dataflow_analysis =
         DFA of {
             id: string,
             ins: 'a list,
             gk: 'b UnorderedSet.set * 'b UnorderedSet.set,
             propSet: 'b UnorderedSet.set,
             diff: bool
         }

fun diffCheck (DFA {diff=d, ...}, diff) = if d then true else diff


fun buildDFAs f cfg =
    if Cfg.fold diffCheck false cfg
    then (Cfg.app f cfg; buildDFAs f cfg)
    else cfg

end
