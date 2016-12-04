{:name "false-belief-task-M2"
 :description "bug"
 :assumptions {                                             ;1 (Common! (forall (?t) (initiates e f ?t)))
               ;2 (Knows! a1 (Knows! a2 (happens e t1)))
               ;3 (Common! (< t1 t2))
               A (forall (?x)
                          (iff (P ?x)
                               (exists (?x) (and (Q ?x) R) )))}

:goal (iff (P a) (exists (?x) (and (Q ?x) R) )) }


