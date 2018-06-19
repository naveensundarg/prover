{:name        "Virtue learning simulation from the point of Agent D"


 :assumptions { ;; A's state of mind.
                P2.1 (Believes! a t1 (holds (state x new) t1))
                P2.2 (Believes! a t2 (holds (state y old) t2))

                ;; D observes a's utterances
                P4.1 (happens (action a (utters (state x new))) (next t1))
                P4.2 (happens (action a (utters (state y old))) (next t2))

                Back (holds (state u old) now)

                Admire (Admire d a)}


 :goal (happens (action  d (utters  (state  u old))) (next now))}