{:name        "Simulation 1"
 :description "Non autonomous agent"
 :assumptions {

               ;;
               C1 (Perceives! a2 (Desires! a1 (holds G t3)))

               C2 (Perceives! a2 (Knows! a1
                                         (necessary
                                           action
                                           G)))


               C3 (Controls a1 a2)

               C4 (requests a1 a2 action)


               NON_AUTONOMOUS_AGENT
                  (forall ( ?agent1 ?agent2 ?goal ?action ?plan ?time)
                          (if (and
                                (Controls ?agent2 ?agent1)
                                (Believes! ?agent1
                                           (Desires! ?agent2 (holds ?goal ?t)))
                                (Believes! ?agent1
                                           (necessary ?action ?goal))
                                (requests ?agent1 ?agent2 ?action))
                            (perform ?agent1 ?action)))

               ;TRUST (iff (trust a2 action goal)
               ;           (@CanProve (perform a2 action)))

               }
 :goal        (and
                (Controls a1 a2)
                (Believes! a2
                           (Desires! a1 (holds G t3)))
                (Believes! a2
                           (necessary action G))
                (requests a1 a2 action))}
