fun collectBBs1 bb =
    let
        fun collectBB (bb as BB {prev=prev, next=next, label=label, ...}, L) =
            if not (isSome (List.find (fn item => item = bb) L)) then
                foldr collectBB (foldr collectBB (bb::L) (!prev)) (!next)
            else L
      ;
    in
        collectBB (bb, [])
    end
;
(*THIS IS RIGHT... we just need some more info*)
(* fun collectBBs L = *)
(*     foldr (fn (bb, L) => L @ (collectBBs1 bb)) [] L *)
  (* ; *)

fun collectBBs L =
    foldr (fn (bb, L) =>
              let
                  val newL = collectBBs1 bb;
              in
                  (print ("Length newL:" ^ (Int.toString (length newL)) ^ "\n");
                   L @ newL)
              end
          ) [] L
;

fun printBB (BB {prev=prev, next=next, label=label, ...}) =
    (print (label ^ ":\nPrevious:");
     app (fn (BB {label=l, ...}) => print (" " ^ l)) (!prev);
     print "\nNext:";
     app (fn (BB {label=l, ...}) => print (" " ^ l)) (!next);
     print "\n\n")
;

fun printCfg ht =
    let
        val bbs = List.rev (collectBBs (map (fn (entry, _) => entry) (HashTable.listItems ht)));
    in
        (print ("LENGTH " ^ (Int.toString (length bbs)) ^ "\n");
         app printBB bbs)
    end
;
