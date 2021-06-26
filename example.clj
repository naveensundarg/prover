{:name        "Per.A "
 :description
 "Perecption"
 :assumptions {:prior1 (e=>  (Perceives! a NOW (and (Red c1) (Red c2) ))
                             (Perceives! a NOW (exists (X) (and (X c1) (X c2) ))))

               :prior2 (e=>  (Perceives! a NOW (and (Green c1) (Green c2) ))
                            (Perceives! a NOW (exists (X) (and (X c1) (X c2)))))

;               :prior2 (e=> (Perceives! a (and (Q d1) (Q d2) ))
;                           (Perceives! a (exists [X] (and (X d1) (X d2)))))


               :input (Perceives! a (and (Q e1) (Q e2)))}
 :goal      (Perceives! a (exists [X] (and (X e1) (X e2))))}


{:name        "IC1.A "
 :description
 "Perecption"
 :assumptions {:prior (e=> (and (IMP (P x) (Q x)) (HOLD (P x)))
                           (HOLD (Q x)))


               :input (and (IMP (P a) (Q a)) (HOLD (P a)))}
 :goal        (HOLD (Q a))}