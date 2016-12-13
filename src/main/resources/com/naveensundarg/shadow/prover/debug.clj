;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Doctrine of Double Effect"
 :description "Simple example of the doctrine of double effect"
 :assumptions {
               1 (Common! (forall ( ?agent ?time )
                                  (initiates (harmed ?agent) (incapacitated ?agent) ?time)))

               2 (Common! (forall ( ?agent ?time )
                                  (initiates (harmed ?agent) (hurt ?agent) ?time)))



               3 (Knows! a1 (<< (value (hurt a2)) (value (incapacitated a2))))

               4 (Knows! a1 (Neutral (harmed a2)))

               a (Knows! a1 (desires (* a1) (incapacitated a2)))

               b (Knows! a1 (not (desires (* a1) (hurt a2)) ) )


               ;; EC Axiom 2: It is common knowledge that if an event happens at t1 and if it initiates f and f is not
               ;; clipped between t1 and t1, then f holds at t2

               A2 (Common! (forall ( ?e ?f ?t1 ?t2)
                                   (if (and (happens ?e ?t1) (initiates ?e ?f ?t1) (< ?t1 ?t2) (not (clipped ?t1 ?f ?t2)))
                                     (holds ?f ?t2))))


               DDE (Common! (forall (?e ?f1 ?f2 ?t)
                                    (if (and (Neutral ?e)
                                             (initiates ?e ?f1 ?t)
                                             (initiates ?e ?f2 ?t)
                                             (<< (value ?f1) (value ?f2))
                                             (desires (* a1) ?f2)
                                             (not (desires (* a1) ?f1)) )
                                     (Permissible ?e))))

               }

 :goal        (Knows! a1 (Permissible (harmed a2)))}

