open TextIO;

fun printDecl ots id = output (ots, "@function " ^ id ^ "\n")


fun printNode ots (l, L) =
    (output (ots, l ^ ":\n");
     app (fn ins => output (ots, "\t" ^ (Iloc.toString ins) ^ "\n")) L)


fun printCfg ots ht =
    (app (printDecl ots) (map (fn (id, _) => id) (HashTable.listItemsi ht));
     output (ots, "\n");
     app (printNode ots) (List.concat (map Cfg.toList (HashTable.listItems ht))))
