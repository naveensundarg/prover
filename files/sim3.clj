
{:description "The adjudicator reasoning about information from the radar."
 :assumptions
 {;;; It is common knowledge at the start what is needed to satisfy clause 2.
   :common (Common! t0
                    (iff clause2
                         (and
                          (exists p
                                  (and (Inside p Building)
                                       (Planning p)))
                          (forall p (if (Inside p Building) (not (Civilian p)))))))

   ;;; Report from the radar.
   :report (Believes! adj t2
                      (Believes! radar t2
                                 (and
                                  (exists p
                                          (and (Inside p Building)
                                               (Planning p)))
                                  (forall p (if (Inside p Building) (not (Civilian p)))))))}

 :goal        (Believes! adj t3 (Believes! radar t2 clause2))}




