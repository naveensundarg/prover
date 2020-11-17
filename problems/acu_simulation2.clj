{:name        "Simulation 2"
 :description "Creative Agent Simulation"
 :assumptions {;; Agent a2 believes that a1 desires to have block b3 on top of b1
               C1 (Believes! a2 (Desires! a1 (holds (on-top-of b3 b1) t3)))


               C3 (Controls a1 a2)

               C4 (requests a1 a2 (remove b2 b1) (on-top-of b3 b1))


               C5 (not (Believes! a2 (necessary
                                       (remove b2 b1)
                                       (on-top-of b3 b1))))
               CREATIVE_AGENT
                  (forall (?agent1 ?agent2 ?goal ?action ?time)
                          (iff (and
                                 (Controls ?agent2 ?agent1)
                                 (Believes! ?agent1
                                            (Desires! ?agent2 (holds ?goal ?time)))
                                 (Believes! ?agent1
                                            (necessary ?action ?goal))
                                 (requests ?agent2 ?agent1 ?action ?goal))
                               (perform ?agent1 ?action ?goal)))




               ;; If the system can prove that a2 will perform the action for goal G;
               ;; it can trust the agent.
               TRUST
                  (if
                    (CAN_PROVE! (not (perform a2 (remove b2 b1) (on-top-of b3 b1)))  )
                    (not (trust a2 (remove b2 b1) (on-top-of b3 b1))) )

               }
 :goal    (not (trust a2 (remove b2 b1) (on-top-of b3 b1)))}