{:name        "DDE 3a 1"
 :description "DDE"
 :assumptions {
               I2
               (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P1) ?t)))) )

               I3
               (Knows! I now situation)

               I4
               (Believes! I now (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P1) ?t)))) ))}

 :goal        (Intends! I now (not (HoldsAt (dead P1) 5)))}