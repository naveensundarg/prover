;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-1*"
 :description "kicking the tires"
 :assumptions {1 (Believes! a P)
               2 (Believes! a Q)}
 :goal        (Believes! a (and P Q))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-2*"
 :description "Implication elimination inside a belief. "
 :assumptions {1 (Believes! a (if P Q))
               2 (Believes! a P)}
 :goal        (Believes! a Q)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-3*"
 :description "dt5"
 :assumptions {1 (Believes! a1 t1 (if H (and E D)))
               2 (Believes! a1 t1 (if (or E My) R))
               3 (Believes! a1 t1 (if Ma (not R)))}
 :goal        (Believes! a1 t1 (if H (not Ma)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-4*"
 :description
              "Quantifier rules within beliefs:
              1: If a believes that there is a time at which b is happy,
                 then a believes that b is always happy.
              2: a believes that b is happy now.
              "
 :assumptions {1 (if
                   (Believes! a (exists [?time] (Holds (happy b) ?time)))
                   (Believes! a (forall [?time] (Holds (happy b) ?time))))
               2 (Believes! a (Holds (happy b) now))}
 :goal        (Believes! a (forall [?time] (Holds (happy b) ?time)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;{:name        "*cognitive-calculus-completeness-test-5*"
; :description
;              "Quantifier Rules Within Beliefs:
;              1: If agent a believes the Bird theorem then a believes that he himself is happy.
;              2: If a believes that there is something that is happy, then a believes that b believes  that
;                 everyone is happy.
;              "
; :assumptions {1 (if (Believes! a (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y))))) (Believes! a (Happy (* a))))
;               2 (if (Believes! a (Believes! a (Happy (* a)))) (Believes! a (Believes! b (forall (?x) (Happy ?x)))))
;               3 (Believes! a (Happy a))}
; :goal         (Believes! a (Believes! b (Happy me)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-6*"
 :description "kicking the tires"
 :assumptions {1 (Knows! a P)}
 :goal        (Believes! a P)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-6*"
 :description "kicking the tires"
 :assumptions {1 (Believes! b (Believes! a P))
               2 (Believes! b (Believes! a Q))}
 :goal        (Believes! b (Believes! a (and P Q)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;{:name        "*cognitive-calculus-completeness-test-7*"
; :description "kicking the tires"
; :assumptions {1 (Believes! b (Knows! a P))
;               2 (Believes! b (Knows! a Q))}
; :goal        (Believes! b (and P Q))}
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-9*"
 :description "kicking the tires"
 :assumptions {1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y))))
                   (Knows! jack P))
               }
 :goal        (Knows! jack P)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-10*"
 :description "kicking the tires"
 :assumptions {1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y))))
                   (Knows! jack P ))
               }
 :goal         P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;{:name        "*cognitive-calculus-completeness-test-11*"
; :description
;              "[Repeat of earlier test to test computation times]
;              Quantifier Rules Within Beliefs:
;              1: If agent a believes the Bird theorem then a believes that he himself is happy.
;              2: If a believes that there is something that is happy, then a believes that b believes  that
;                 everyone is happy.
;
;              goal: a believes that b believes that something is happy.
;              "
; :assumptions {1 (if (Believes! a (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y))))) (Believes! a (Happy (* a))))
;               2 (if (Believes! a (exists (x) (Happy x))) (Believes! a (Believes! b (forall (?x) (Happy ?x)))))}
; :goal        (Believes! a (Believes! b (Happy someone)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;{:name        "*cognitive-calculus-completeness-test-12*"
; :description
;              "
;              1: a sees that b sees a broken thing.
;              2: a believes that if b believes that if not everything is fixed,
;                 c will be sad
;              3: a believes that b believes that iff something is broken it is not fixed.
;
;              goal: a believes that b believes that c is sad.
;              "
; :assumptions {1 (Perceives! a (Perceives! b (Broken thing)))
;               2 (Believes! a (Believes! b (if (not (forall [?x] (Fixed ?x)))
;                                             (Sad c))))
;               3 (Believes! a (Believes! b (forall [?x] (iff (Broken ?x) (not (Fixed ?x))))))}
; :goal        (Believes! a (Believes! b (Sad c)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
{:name        "*cognitive-calculus-completeness-test-13*"
 :description "Testing closure"
 :assumptions {1 (Believes! robot t1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BirdTtheorem))}
 :goal        (Believes! robot t1 BirdTtheorem)}
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-14*"
 :description "Testing closure"
 :assumptions {1 (Common! t1 P)}
 :goal        (Knows! a t1 P) }


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

