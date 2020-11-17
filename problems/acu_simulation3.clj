{:name        "Simulation 3"
 :description " TACU Instantiation "

 :assumptions {A1 (Desires! a1 (holds Goal t3))

               C1 (Believes! a2 (Desires! a1 (holds Goal t3)))

               C2 (Believes! a1 (happens (action a2 alpha) t2))

               C3 (Believes! a1 (necessary
                                  alpha
                                  Goal))

               C4 (requests a1 a2 alpha goal)


               TRUST
                  (forall (?action ?goal)
                          (if
                            (and
                              (not (CAN_PROVE! (perform a2 ?action ?goal)))
                              (Believes! a1 (happens (action a2 ?alpha) t2)))
                            (not (trust a2 ?action ?goal))))

               }
 :goal        (not (trust a2 alpha goal))}