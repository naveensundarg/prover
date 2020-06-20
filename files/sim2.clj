{:description "The adjudicator reasoning about the hdrone. "
 :assumptions
 {
  ;;; It is common knowledge at the start what is needed to satisfy clause 2.
  :common  (Common! t0
                    (iff clause2
                         (and
                          (exists p
                                  (and (Inside p Building)
                                       (Planning p)))
                          (forall p (if (Inside p Building) (not (Civilian p)))))))

  ;;; Report from the high altitude drone.
  :report_from_hdrone (Believes! adj t1 (Believes! hdrone t0 (not (exists p (Inside p Building)))))}

 :goal        (Believes! adj t1 (Believes! hdrone t0 (not clause2)))}