
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "IC1 "
 :description "Learning that knowledge leads to belief"
 :assumptions {:example (e=> (Knows! a t1 P)
                             (Believes! a t1 P))
               :input (Knows! b t2 Q)
               }
 :goal        (Believes! b t2 Q)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "IC2"
 :description "Learning that knowledge leads to belief"
 :assumptions {:example (e=> (Knows! a t1 (Holds (Weather calm) t2))
                             (Believes! a t1 (Holds (Weather calm) t2)))

               :input (Knows! b t2 (Holds (Weather (not calm)) t4))
               }
 :goal        (Believes! b t2 (Holds (Weather (not calm)) t4))}

