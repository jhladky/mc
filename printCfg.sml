fun printNode (l, L) =
    (print (l ^ ":\n");
     app (fn ins => print ("\t" ^ (Iloc.toString ins) ^ "\n")) L)


fun printCfg ht =
    app printNode (List.concat (map Cfg.toList (HashTable.listItems ht)))
