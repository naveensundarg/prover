{

 SC1 (forall [?f ?t]
             (implies (and (InitiallyP ?f) (not (Clipped t0 ?f ?t)))
                      (HoldsAt ?f ?t)))

 SC2 (forall [?t1 ?t2 ?e ?f]
             (implies (and (Happens ?e ?t1)
                           (Initiates ?e ?f ?t1)
                           (Prior ?t1 ?t2)
                           (not (Clipped ?t1 ?f ?t2)))
                      (HoldsAt ?f ?t2)))

 SC3 (forall [?t1 ?f ?t2]
             (iff (Clipped ?t1 ?f ?t2)
                  (exists [?e ?t]
                          (and (Happens ?e ?t)
                               (Prior ?t1 ?t)
                               (Prior ?t ?t2)
                               (Terminates ?e ?f ?t)))))
 }