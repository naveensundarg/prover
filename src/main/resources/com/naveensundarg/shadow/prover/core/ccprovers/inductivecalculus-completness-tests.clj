
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "IC1.A "
 :description "Learning that knowledge leads to belief with an atomic example."
 :assumptions {:example (e=> (Knows! a t1 P)
                             (Believes! a t1 P))
               :input (Knows! b t2 Q)
               }
 :goal        (Believes! b t2 Q)}

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "IC1.B"
 :description "Learning that knowledge leads to belief with a first order example."
 :assumptions {:example (e=> (Knows! a t1 (Holds (Weather calm) t2))
                             (Believes! a t1 (Holds (Weather calm) t2)))

               :input (Knows! b t2 (Holds (Weather (not calm)) t4))
               }
 :goal        (Believes! b t2 (Holds (Weather (not calm)) t4))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "IC1.C"
 :description "Learning that knowledge leads to belief with a modal first-order example."
 :assumptions {:example (e=> (Knows! a t1 (Knows! c (Holds (Weather calm) t2)))
                             (Believes! a t1 (Knows! c (Holds (Weather calm) t2))))

               :input (Knows! b t2 (Holds (Weather (not calm)) t4))
               }
 :goal        (Believes! b t2 (Holds (Weather (not calm)) t4))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "IC21."
 :description "Learning that knowledge is true."
 :assumptions {:example (e=> (Knows! a t1 P) P)
               :input (Knows! b t2 Q)
               }
 :goal        Q}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "IC2.2"
 :description "Learning that knowledge is true.
               Nested knowledge is also true."
 :assumptions {:example (e=> (Knows! a t1 P) P)
               :input (Knows! c t3 (Knows! d t4 (Knows! e t5 R)))
               }
 :goal        R}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "IC3.1"
 :description "Learning two inference schemata: knowledge is true and knowledge is belief."
 :assumptions {:example1 (e=> (Knows! a t1 P) P)
               :example2 (e=> (Knows! b t2 Q)
                              (Believes! b t2 Q))
               :input (Knows! c t3 R)
               }
 :goal        (and R (Believes! c t3 R))}
