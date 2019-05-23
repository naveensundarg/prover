{:name        "Virtue learning simulation from the point of Agent D"


 :assumptions { ;; A's state of mind.
                P1 (Believes! I now (and
                                     (Knows! a t1 (holds (state x new) t1))

                                     (Knows! a t2 (holds (state y old) t2))))

                ;; D observes a's utterances
                P2 (Perceives! a t1 (happens
                                     (action a (utters (state x new))) (next t1)))

                P3 (Perceives! a t2 (happens
                                     (action a (utters (state y old))) (next t2)))

                Background (Believes! I t0 (holds (state u old) now))

                Admire (Admire d a)}


 :goal (happens
        (action  d
                 (utters  (state  u old)))
        (next now))




 }