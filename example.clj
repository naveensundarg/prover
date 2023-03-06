
{:name        "ALML to Reasoning over Perception  "
 :description
 "If the car knows that there is an object infront of it and it perceives itself not perceiving the object, the car then
  knows that its perception is immpaired"
 :assumptions {:given1 ()

               }
 :goal        (Knows! car (Impaired perception car))}


;
;
;
;
;
;{:name        "Perception of rule violation "
; :description
; "Can we violate a rule in this situation"
; :assumptions {:prior1  (e=>
;
;                           (Perceives! a NOW
;                                       (and (not (RuleLeftTurn c1))
;                                            (not (RuleLeftTurn c2))
;                                            (not (RuleLeftTurn c3))))
;
;                         (Perceives! a NOW
;                                     (exists (X)
;                                             (and (not (X c1)) (not (X c2)) (not (X c3))))))
;
;               :prior4  (e=> (Knows! ?x ?y ?P) ?P)
;               :prior5  (e=> (Perceives! ?x ?y ?P) ?P)
;
;               :violable (Perceives! a NOW
;                                (exists (X)
;                                        (if  (and (not (X c1)) (not (X c2)) (not (X c3)))
;                                          (CanViolate X))))
;
;               :input (Perceives! a NOW
;                                      (and (not (RuleRightTurn c1))
;                                           (not (RuleRightTurn c2))
;                                           (not (RuleRightTurn c3))))
;
;                         }
; :goal        (CanViolate RuleRightTurn)}
;
;
;{:name        "Per.A "
; :description
; "Perecption"
; :assumptions {:prior1 (e=>
;                        (holds (location car1 (position -1)) t1)
;                        (happens (action car1 (turnInto (lane 1))) (next t1)))
;
;               :prior2 (e=>
;                        (holds (location car2 (position -2)) t3)
;                        (happens (action car2 (turnInto (lane 1))) (next t3)))
;
;               :prior3 (e=>
;                        (holds (location car3 (position -3)) t5)
;                        (happens (action car3 (turnInto (lane 1))) (next t5)))
;
;               :prior4 (e=> (Knows! ?x ?y ?P) ?P)
;
;
;               :input2 (Knows! self (holds (location self (position 0)) t7))}
; :goal        (happens (action self (turnInto (lane 1))) (next t7))}
;
;
;{:name        "Per.A "
; :description
; "Perecption"
; :assumptions {:prior1 (e=>
;                        (and (Perceives! a NOW (and (Red c1) (Red c2) (Red c3)))
;                             (Knows! a (Color Red)))
;                        (Perceives! a NOW
;                                    (exists (X)
;                                            (and (X c1) (X c2) (X c3)
;                                                 (Color X)))))
;
;               :prior2 (e=>
;                        (and (Perceives! a NOW (and (Green c1) (Green c2) (Green c3)))
;                             (Knows! a (Color Green)))
;                        (Perceives! a NOW
;                                    (exists (X)
;                                            (and (X c1) (X c2) (X c3)
;                                                 (Color X)))))
;
;
;               :input  (and (Perceives! a (and (Blue e1) (Blue e2) (Blue e3)))
;                            (Knows! a (Color Blue)))}
; :goal        (Perceives! a NOW
;                          (exists (X)
;                                  (and (X e1) (X e2) (X e3)
;                                       (Color X))))}
;
;
;{:name        "IC1.A "
; :description
; "Perecption"
; :assumptions {:prior (e=> (and (IMP (P x) (Q x)) (HOLD (P x)))
;                           (HOLD (Q x)))
;
;
;               :input (and (IMP (P a) (Q a)) (HOLD (P a)))}
; :goal        (HOLD (Q a))}
;
;
