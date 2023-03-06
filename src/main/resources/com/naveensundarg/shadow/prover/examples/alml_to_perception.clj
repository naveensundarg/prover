{:name        "ALML to Reasoning over Perception  "
 :description
 "If the car knows that there is an object infront of it and it perceives itself not perceiving the object, the car then
  knows that its perception is immpaired. If the car's perception is impaired, the car then learns from two examples, that it
  can only form beliefs with strength 1 from impaired perception."
 :assumptions {:given1 (Perceives! car
                                   (not
                                    (Perceives! car (InfrontOf car thing))))

               :given2 (Knows! car
                               (and (InfrontOf car thing)
                                    (Light thing)))
               :given3 (Knows! car
                               (if  (and
                                     (and (InfrontOf car thing)
                                          (Light thing))
                                     (not (Perceives! car (InfrontOf car thing))))
                                 (Impaired perception car)))

               :given4 (if (Knows! car (Impaired perception car))
                         (and
                          (e=> (ImpairedPerception car P) ($ 1 (Believes! car P)))
                          (e=> (ImpairedPerception car Q) ($ 1 (Believes! car Q)))))
               :impaired-perception
               (ImpairedPerception car FreeIntersection)}

 :goal        ($ 1 (Believes! car FreeIntersection))}
