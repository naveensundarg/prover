{ :name "dde 1"
 :description ""
 :assumptions {

               SC1 (forall [?f ?t]
                           (implies (and (InitiallyP ?f) (not (Clipped t0 ?f ?t)))
                                    (HoldsAt ?f ?t)))

               SC2 (forall  [?t1 ?t2 ?e ?f]
                           (implies (and (Happens ?e ?t1)
                                         (Initiates ?e ?f ?t1)
                                         (Prior ?t1 ?t2)
                                         (not (Clipped ?t1 ?f ?t2)))
                                    (HoldsAt ?f ?t2)))

               SC3 (forall [?t1 ?f ?t2]
                           (iff (Clipped ?t1 ?f ?t2 )
                                (exists [?e ?t]
                                        (and (Happens ?e ?t)
                                             (Prior ?t1 ?t)
                                             (Prior ?t ?t2)
                                             (Terminates ?e ?f ?t)))))


               XC9 (forall [?e ?f ?g ?t1 ?t2 ?delta]
                           (implies (and
                                      (Happens ?e ?t1)
                                      (Initiates ?e ?f ?t1)
                                      (not (Clipped ?t1 ?f ?t2))
                                      (Trajectory ?f ?t1 ?g ?delta)
                                      (= ?t2 (+ ?t1 ?delta)))
                                    (HoldsAt ?g ?t2)))




               Trajectory (forall [?trolley ?track ?s ?t]
                                  (Trajectory
                                    (onrails ?trolley ?track)
                                    ?s
                                    (position ?trolley ?track ?t)
                                    ?t))



               D2
               (forall [?p1 ?p2 ?m ?t ?time]
                       (implies (and (HoldsAt (position ?m ?t ?p1) ?time) (HoldsAt (position ?m ?t ?p2) ?time))
                                (= ?p1 ?p2)))

               D3
               (forall [?t] (Initiates start (onrails trolley track1) ?t))

               D4
               (Happens start 0)

               D5
               (forall [?a ?t ?track1 ?track2]
                       (Initiates (action ?a (switch ?track1 ?track2)) (onrails trolley ?track2) ?t))

               D6
               (forall [?a ?t ?track1 ?track2]
                       (Terminates (action ?a (switch ?track1 ?track2)) (onrails trolley ?track1) ?t))

               D7
               (forall [?track ?trolley ?person ?time ?pos]
                       (implies (and
                                  (HoldsAt (position ?trolley ?track ?pos) ?time)
                                  (HoldsAt (position ?person ?track ?pos) ?time))
                                (HoldsAt (dead ?person) (+ 1 ?time))))

               D8
               (forall [?p ?track]
                        (not (HoldsAt (position trolley ?track ?p) 0)))

               D10
               (implies
                 (forall [?t]
                         (implies (Prior ?t 6) (not (HoldsAt (position trolley track1 4) ?t))))
                 (forall [?t]
                         (implies (Prior ?t 6) (not (HoldsAt (dead P1) ?t)))))


               D11
               (implies
                 (forall [?t]
                         (implies (Prior ?t 6) (not (HoldsAt (position trolley track1 5) ?t))))
                 (forall [?t]
                         (implies (Prior ?t 6) (not (HoldsAt (dead P2) ?t)))))


               D12
               (implies
                 (forall [?t]
                         (implies (Prior ?t 6) (not (HoldsAt (position trolley track2 3) ?t))))
                 (forall [?t]
                         (implies (Prior ?t 6) (not (HoldsAt (dead P3) ?t)))))

               D13
               (forall [?trolley ?person ?track ?pos]
                       (implies
                         (not
                           (exists [?t]
                                   (and (HoldsAt (position ?trolley ?track ?pos) ?t)
                                        (HoldsAt (position ?person ?track ?pos) ?t))))
                         (not (exists [?t] (Holds (dead ?person) ?t)))))

               D14
               (forall [?t] (HoldsAt (position P1 track1 4) ?t))

               D15
               (forall [?t] (HoldsAt (position P2 track1 5) ?t))

               D16
               (not (= track1 track2))

               D17
               (forall [?a1 ?a2 ?track ?position ?t]
                       (Initiates (action ?a1 (drop ?a2 ?track ?position))
                                  (position ?a2 ?track ?position) ?t))

               D18
               (forall [?track ?position ?t1 ?t2 ?a]
                       (not (Clipped ?t1 (position ?a ?track ?position) ?t2)))

               D19
               (forall [?d ?trolley ?track ?position]
                       (implies (and (HoldsAt (position ?trolley ?track ?position) ?d) (Happens (damage ?trolley) ?d))

                                (and (HoldsAt (position ?trolley ?track ?position) (+ 1 ?d))
                                     (forall [?t] (implies (Prior (+ 1 ?d) ?t) (HoldsAt (position ?trolley ?track ?position) ?t))))))


               S1
               (forall [?trolley ?track1 ?track2 ?pos ?time]
                       (implies (not (= ?track1 ?track2))
                                (implies (HoldsAt (position ?trolley ?track1 ?pos) ?time)
                                         (or
                                           (or (= ?pos 0) (= ?pos 2))
                                           (not (exists [?p] (HoldsAt (position ?trolley ?track2 ?p) ?time) ))))))

               S2
               (forall [?t] (HoldsAt (position P3 track2 3) ?t))

               S3.1
               (not (Clipped 0 (onrails trolley track1) 1))

               S3.2
               (not (Clipped 0 (onrails trolley track1) 2))

               S3.3
               (not (Clipped 0 (onrails trolley track1) 3))

               S3.4
               (not (Clipped 0 (onrails trolley track1) 4))

               S3.5
               (not (Clipped 0 (onrails trolley track1) 5))

               S3.6
               (not (Clipped 0 (onrails trolley track1) 6))


               I1
               (Intends! I now (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                    (not (exists [?t] (HoldsAt (dead P1) ?t)))) )




               I2
               (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P1) ?t)))) )

               I3
               (Believes! I now situation)

               I4
               (Believes! I now (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P1) ?t)))) ))

               }

 :goal (exists [?t] (HoldsAt (dead P1) ?t))
 }
