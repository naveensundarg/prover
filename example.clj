{:name        "IC1.A "
 :description
 "Learning that knowledge leads to belief with an atomic example.
 DR5 in http://kryten.mm.rpi.edu/CognitiveCalculus092808.pdf"
 :assumptions {:prior (e=> (and P

                             (forall gamma (iff (rho (NUM gamma)) gamma)))
                        (exists chi (if (forall gamma (iff (rho (NUM gamma)) gamma))
                                      (and (not (rho chi)) (not (rho (not chi)))))))


               :input  (and P

                            (forall gamma (iff (rho (NUM gamma)) gamma)))

               }
 :goal       (exists chi (if (forall gamma (iff (rho (NUM gamma)) gamma))
                           (and (not (rho chi)) (not (rho (not chi))))))}