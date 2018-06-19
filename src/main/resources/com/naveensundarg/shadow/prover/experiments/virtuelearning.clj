{:name        "Virtue learning simulation from the point of Agent D"


 :assumptions { ;; A's state of mind.
                P2.1 (Believes! a t1 (holds (state x old) t1))
                P2.2 (Believes! a t2 (holds (state y old) t2))

                ;; D observes a's utterances
                P4.1 (happens (action a (utter (state x old))) (next t1))
                P4.1 (happens (action a (utter (state y old))) (next t2))

                Back (holds (state u old) now)

                Admire (Admire d a)
                Trait (Trait! [?obj ?value ?time] a now (holds (state ?obj ?value) ?time)
                             (utters (state ?obj ?value)))

                }
;;(happens (action  d (utters  (state  u old))) now)
 :goal (happens (action  d (utters  (state  u old))) (next now))
 }