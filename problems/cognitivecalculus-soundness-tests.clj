{:name        "cc-soundness-test-1"
 :description "kicking the tires"
 :assumptions {1 A}
 :goal        (or P Q)
 }

{:name        "cc-soundness-test-2"
 :description "Referential opacity should be satisfied"
 :assumptions {1 (not (Knows! a now (= morning_star evening_star)))
               2 (= morning_star evening_star)
               3 (Knows! a now (= morning_star morning_star))}
 :goal        (and P (not P))
 }


{:name        "DDE F1 S1"
 :description "DDE"
 :assumptions {
               I2
               (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P2) ?t)))) )

               I3
               (Knows! I now situation)

               I4
               (Believes! I now (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                                             (not (exists [?t] (HoldsAt (dead P2) ?t)))) ))}

 :goal        (not (Ought! I 2 situation (not (happens (action I (switch track1 track2)) 2)))) }

{:name        "DDE F1 S2"
 :description "DDE"
 :assumptions {
               I2
               (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P2) ?t)))) )

               I3
               (Knows! I now situation)

               I4
               (Believes! I now (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                                             (not (exists [?t] (HoldsAt (dead P2) ?t)))) ))}

 :goal        (not (Ought! I 2 situation (not (Happens (action I (drop P3 track1 3   )) 1 )))) }


{:name        "DDE 3b"
 :description "DDE"
 :assumptions {
               I2
               (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P2) ?t)))) )

               I3
               (Knows! I now situation)

               I4
               (Believes! I now (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                                             (not (exists [?t] (HoldsAt (dead P2) ?t)))) ))}

 :goal        (Intends! I now (HoldsAt (dead P3) 6))}