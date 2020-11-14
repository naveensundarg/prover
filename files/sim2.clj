
{:description "The adjudicator reasoning about the faulty ldrone. "
 :assumptions
 {;; It is common knowledge that if an agent reports inconsistent information,
 ;; then as protective measure, we assume that the agent has no belief either way.
   :inconsistent (Common! t0
                          (if (Believes! ldrone t1 False)
                            (and (not (Believes! ldrone t1 clause2))
                                 (not (Believes! ldrone t1 (not clause2))))))
   ;;; Report from the high altitude drone.
   :report       (Believes! a t1
                            (Believes! ldrone t1
                                       (exists p
                                               (and (Inside p Building)
                                                    (not (Inside p Building))))))}

 :goal        (Believes! a t2
                         (and (not (Believes! ldrone t1 clause2))
                              (not (Believes! ldrone t1 (not clause2)))))}





