open TextIO;


fun printDecl ots (id, _) = output (ots, "@function " ^ id ^ "\n")


fun printNode ots (l, L) =
    (output (ots, l ^ ":\n");
     app (fn ins => output (ots, "\t" ^ (Iloc.toString ins) ^ "\n")) L)


fun printCfg ots ht =
    (HashTable.appi (printDecl ots) ht;
     output (ots, "\n");
     app (printNode ots) (List.concat (map Cfg.toList (HashTable.listItems ht))))
