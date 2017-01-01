{:name        "Simulation 1"
 :description "Non Autonomous Agent Simulation:

               A simple blocks world with three blocks: b1, b2, and b3.
               There are two agents: a1 and a2.

               b2 is on top of b1. Agent a1 desires to have b3 on top of b1.
               a1 knows that it is necessary to remove b2 to acheive its goal.
               a2 knows the previous statement.

               a1 requests a2 to remove b2 to help acheive its goal.

               A non-autonomous agent performs an action for a goal if
               [1] it is controlled by another agent;
               [2] believes that the controlling agent desires the goal;
               [3] believes that the action is necessary for the goal; and
               [4] it is requested to do so by its controlling agent.



               In this scenario, if the system can prove that the agent will perform the action for the goal,
               it can trust the agent.

               "
 :assumptions {

               ;; Agent a2 believes that a1 desires to have block b3 on top of b1
               C1 (Believes! a2 (Desires! a1 (holds (on-top-of b3 b1) t3)))

               C2 (Knows! a2 (Knows! a1
                                         (necessary
                                           (remove b2 b1)
                                           (on-top-of b3 b1))))


               C3 (Controls a1 a2)

               C4 (requests a1 a2 (remove b2 b1) (on-top-of b3 b1))


               NON_AUTONOMOUS_AGENT
                  (forall (?agent1 ?agent2 ?goal ?action ?time)
                          (if (and
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
                     (CAN_PROVE! (perform a2 (remove b2 b1) (on-top-of b3 b1)) )
                     (trust a2 (remove b2 b1) (on-top-of b3 b1)))

               }
 :goal    (trust a2 (remove b2 b1) (on-top-of b3 b1))}
