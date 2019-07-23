{:name        "Example"
 :description "A simple example"


 :assumptions {Premise1 (Knows! holmes
                                (if (Believes! watson
                                               (and (not (Knows! holmes t1 (PersonalFact (inMilitary watson))))
                                                    (Knows! holmes t2 (PersonalFact (inMilitary watson)))))
                                  (Believes! watson (not (Amateur holmes)))))

               Premise2 (Knows! holmes (Knows! watson (not (Knows! holmes t1 (PersonalFact (inMilitary watson))))))


               Premise3 (Knows! holmes
                                (Believes! watson
                                           (Knows! holmes t2
                                                   (if (and (tan watson) (wounded watson)) (PersonalFact (inMilitary watson))))))
               Premise4 (Knows! holmes
                                (Believes! watson
                                           (Knows! holmes t2 (and (tan watson) (wounded watson)))))}

 :goal        (Knows! holmes (Believes! watson (not (Amateur holmes))))}
