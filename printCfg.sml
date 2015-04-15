fun printDecl id = print ("@function " ^ id ^ "\n")


fun printNode (l, L) =
    (print (l ^ ":\n");
     app (fn ins => print ("\t" ^ (Iloc.toString ins) ^ "\n")) L)


fun printCfg ht =
    (app printDecl (map (fn (id, _) => id) (HashTable.listItemsi ht));
     print "\n";
     app printNode (List.concat (map Cfg.toList (HashTable.listItems ht))))
