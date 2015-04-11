local
    fun collectBB (bb as BB {prev=prev, next=next, label=label, ...}, L) =
        if not (isSome (List.find (fn item => item = bb) L)) then
            foldr collectBB (foldr collectBB (bb::L) (!prev)) (!next)
        else L
    ;

    fun collectBBs1 bb = collectBB (bb, []);
in
    fun collectBBs L =
        foldr (fn (bb, L) => L @ (collectBBs1 bb)) [] L
    ;
end;

fun printBB (BB {prev=prev, next=next, label=label, ...}) =
    (print (label ^ ":\nPrevious:");
     app (fn (BB {label=l, ...}) => print (" " ^ l)) (!prev);
     print "\nNext:";
     app (fn (BB {label=l, ...}) => print (" " ^ l)) (!next);
     print "\n\n")
;

fun printCfg ht =
    app printBB ((List.rev o collectBBs) (map (fn (CFG {entry=e, ...}) => e)
                                              (HashTable.listItems ht)))
;
