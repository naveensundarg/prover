
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-1*"
 :description "kicking the tires"
 :assumptions {1 (Knows! a1 t1 P)}
 :goal        P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "QAV-1"
 :description "Quantification across modals"
 :assumptions {

                A1 (forall [?x] (if
                                  (Believes! john (Human ?x))
                                  (Believes! john (Mortal ?x))))
                A2 (Believes! john (Human mary))
                }

 :goal        (Believes! john (Mortal mary))
 }


{:name        "QAV-2"
 :description "Quantification across modals"
 :assumptions {

                A1 (forall [?agent ?x] (if
                                  (Believes! ?agent (Human ?x))
                                  (Believes! ?agent (Mortal ?x))))
                A2 (Believes! john (Human mary))
                }

 :goal        (Believes! john (Mortal mary))
 }

{:name        "QAV-3"
 :description "Quantification across modals"
 :assumptions {

                A1 (forall [?agent ?t ?x] (if
                                         (Believes! ?agent ?t (Human ?x))
                                         (Believes! ?agent (next ?t) (Mortal ?x))))
                A2 (Believes! john t1 (Human mary))
                }

 :goal        (Believes! john (next t1) (Mortal mary))
 }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-1-a*"
 :description "testing short hand rules"
 :assumptions {1 (Knows! a1 P)}
 :goal        (Believes! a1 now P)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-2*"
 :description "kicking the tires"
 :assumptions {1 (Believes! a1 t0 P)
               2 (Believes! a1 t0 (if P Q))}
 :goal        (Believes! a1 t0 Q)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "conjunction-test-1"
 :description "Conjunctions"
 :assumptions {1 (Common! t0 P)}
 :goal        (and (Knows! a1 t1 P) (Knows! a2 t1 P))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "conjunction-test-2"
 :description "Conjunctions"
 :assumptions {1 (and (Common! t0 P) (Common! t0 Q))}
 :goal        (Knows! a1 t1 (and P Q))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "conjunction-test-3"
 :description "Conjunctions"
 :assumptions {1 (and (Common! t0 (forall (?x) (if (human ?x) (mortal ?x))))
                      (Common! t0 (human socrates)))}
 :goal        (Knows! a1 t1 (mortal socrates))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-3*"
 :description "Bird Theorem and Jack"
 :assumptions {1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y))))
                   (Knows! jack t0 BirdTheorem))}
 :goal        (Knows! jack t0 BirdTheorem)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-3*"
 :description "Bird Theorem and Jack"
 :assumptions {1 (Believes! a P)
               2 (Believes! a Q)
               3 (if (Believes! a (and P Q)) (Knows! a R))}
 :goal        R}


{:name        "*cognitive-calculus-completeness-test-5*"
 :description "dt5"
 :assumptions {1 (Knows! a1 t1 (if H (and E D)))
               2 (Knows! a1 t1 (Knows! a2 t2 (if (or E My) R)))
               3 (Knows! a1 t1 (Knows! a2 t2 (Knows! a3 t2 (if Ma (not R)))))}
 :goal        (if H (not Ma))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-6*"
 :description "dt6"
 :assumptions {1 (and P (Knows! a t0 Q))}
 :goal        Q}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-7*"
 :description "dt6.a"
 :assumptions {1 (and P (Knows! a t0 Q))}
 :goal        (and P Q)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-8*"
 :description "dt6.a"
 :assumptions {1 (and P (Knows! a t0 Q))}
 :goal        (or P Q)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-9*"
 :description "dt7"
 :assumptions {1 (and P (Knows! a now (and Q (Knows! b now R2))))
               2 (and P (Knows! a now (and Q (Knows! b now R1))))}
 :goal        (and R1 R2)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-10*"
 :description "dt8"
 :assumptions {1 P
               2 (if P (Knows! a now Q))}
 :goal        Q}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;{:name        "*cognitive-calculus-completeness-test-11*"
; :description "dt8.a"
; :assumptions {1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) (Knows! a now Q))}
; :goal        Q}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-12*"
 :description "dt10"
 :assumptions {1 (or (Knows! a now P) (Knows! b now P))}
 :goal        P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-13*"
 :description "dt11"
 :assumptions {1 (Believes! a t1 (if P (Knows! b now Q)))
               2 (Believes! a t2 P)}
 :goal        (Believes! a t2 (Knows! b now Q))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-14*"
 :description "dt11.a"
 :assumptions {1 (if P (Knows! b now (and (Knows! c t1 Q1) (Knows! c t2 Q2))))
               2 (or (Knows! a now P) (Knows! b now P))}
 :goal        (and Q1 Q2)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-16*"
 :description "dt15"
 :assumptions {1 (if P
                   (Knows! jack now (not (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))))))}
 :goal        (not P)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-17*"
 :description "dt16 check DR1"
 :assumptions {1 (Common! now (Common! now P))}
 :goal        P}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-3*"
 :description "Bird Theorem and Jack"
 :assumptions {1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y))))
                   (Knows! jack t0 BirdTheorem))}
 :goal        (Knows! jack t0 BirdTheorem)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-ought-test-1*"
 :description "Testing the ought rule"
 :assumptions {1 (Believes! jack t0 P)
               2 (Believes! jack t0 (Ought! jack t0 P (happens (action jack A) t0)))}
 :goal        (happens (action jack A) t0)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-ought-injured-test-2*"
 :description "Testing the ought rule"
 :assumptions {1 (Believes! robot now (if (cries soldier) (injured soldier)))
               2 (Believes! robot now (cries soldier))
               3 (Believes! robot now
                            (Ought! robot now (injured soldier) (happens (action robot (help soldier)) now)))}
 :goal        (happens (action robot (help soldier)) now)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-ought-injured-test-3*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! now (if (cries soldier) (injured soldier)))
               2 (Knows! robot now (cries soldier))
               3 (Common! now
                          (Ought! robot now (injured soldier) (happens (action robot (help soldier)) now)))}
 :goal        (happens (action robot (help soldier)) now)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-ought-injured-test-3-complex*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! t0 (forall (?x) (if (cries ?x) (injured ?x))))
               2 (Knows! robot t1 (cries soldier))
               3 (Common! t2
                          (Ought! robot t2 (injured soldier) (happens (action robot (help soldier)) t2)))}
 :goal        (happens (action robot (help soldier)) t2)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "*cognitive-calculus-ought-injured-test-4*"
 :description "Testing the ought rule"
 :assumptions {1 (Believes! robot now INJURED)
               2 (Common! now (Ought! robot now INJURED (happens (action robot HELP) now)))}
 :goal        (happens (action robot HELP) now)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-ought-injured-test-4*"
 :description "Testing the ought rule"
 :assumptions {1 (Believes! robot now INJURED)
               2 (Common! now (Ought! robot now INJURED (happens (action robot HELP) now)))}
 :goal        (happens (action robot HELP) now)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-1*"
 :description "Testing closure"
 :assumptions {1 (Believes! robot t1 (forall (?x) (if (Man ?x) (Mortal ?x))))
               2 (Believes! robot t2 (Man socrates))}
 :goal        (Believes! robot t3 (Mortal socrates))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-2*"
 :description "Testing closure"
 :assumptions {1 (Common! t1 (forall (?x) (if (Man ?x) (Mortal ?x))))
               2 (Common! t2 (Man socrates))}
 :goal        (Believes! robot t1 (forall (?x) (if (Man ?x) (Mortal ?x))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-3*"
 :description "Testing closure"
 :assumptions {1 (Believes! robot t1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BirdTtheorem))}
 :goal        (Believes! robot t2 BirdTtheorem)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-4*"
 :description "Testing closure"
 :assumptions {1 (Believes! robot1 t1
                            (Believes! robot2 t1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BirdTtheorem)))}
 :goal        (Believes! robot1 t2 (Believes! robot2 t1 BirdTtheorem))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-5*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! t0 (forall (?x) (if (and (honest ?x) (criesForHelp ?x)) (injured ?x))))
               ;; It is commonly known that if someone is honest and cries for help, then they are injured.

               2 (Common! t1 (Ought! robot2 t2 (injured mary) (happens (action robot2 (help mary)) t2)))

               3 (Believes! robot1 t0 (Believes! robot2 t0 (honest mary)))

               4 (Common! t1 (criesForHelp mary))}
 :goal        (Believes! robot1 t2 (happens (action robot2 (help mary)) t2))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-6*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! t0 (forall (?x) (if (and (honest ?x) (criesForHelp ?x)) (injured ?x))))
               ;; It is commonly known that if someone is honest and cries for help, then they are injured.

               2 (Common! t1 (Ought! robot2 t2 (injured mary) (happens (action robot2 (help mary)) t2)))

               3 (Believes! robot1 t0 (Believes! robot2 t0 (honest mary)))

               4 (Common! t1 (criesForHelp mary))}
 :goal        (Believes! robot1 t2 (happens (action robot2 (help mary)) t2))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*licato*"
 :description "from licato's paper"
 :assumptions {1 (Knows! a t (or (isExit A) (isExit B)))

               2 (Perceives! a t (not (isExit A)))}
 :goal        (Knows! a t (isExit B))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "universal intro inside a knows"
 :description ""
 :assumptions {1 (forall (?x) (if (P ?x) (Knows! ?x U)))
               2 (P a)}

 :goal        (Knows! a U)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "false-belief-task-M1"
 :description "Method 1 in the False Belief Task Paper"
 :assumptions {1 (Perceives! a1 (happens (action a2 alpha) t))
               2 (Common!
                   (forall (?a ?alpha ?t)
                           (if (happens (action ?a ?alpha) ?t)
                             (Knows! ?a (happens (action ?a ?alpha) ?t)))))}

 :goal        (Knows! a1 (Knows! a2 (happens (action a2 alpha) t)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  {:name        "DDE base"
   :description "DDE"
   :assumptions {I2
                 (Ought! I now situation
                         (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                              (not (exists [?t] (HoldsAt (dead P1) ?t)))))

                 I3
                 (Knows! I now situation)

                 I4
                 (Believes! I now
                            (Ought! I now situation
                                    (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                         (not (exists [?t] (HoldsAt (dead P1) ?t))))))}

   :goal        (Intends! I now
                          (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                               (not (exists [?t] (HoldsAt (dead P1) ?t)))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "DDE 3a 1"
 :description "DDE"
 :assumptions {I2
               (Ought! I now situation
                       (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                            (not (exists [?t] (HoldsAt (dead P2) ?t)))))

               I3
               (Knows! I now situation)

               I4
               (Believes! I now
                          (Ought! I now situation
                                  (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                       (not (exists [?t] (HoldsAt (dead P2) ?t))))))}

 :goal        (Intends! I now (not (HoldsAt (dead P1) 5)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "DDE 3a 2"
 :description "DDE"
 :assumptions {I2
               (Ought! I now situation
                       (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                            (not (exists [?t] (HoldsAt (dead P2) ?t)))))

               I3
               (Knows! I now situation)

               I4
               (Believes! I now
                          (Ought! I now situation
                                  (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                       (not (exists [?t] (HoldsAt (dead P2) ?t))))))}

 :goal        (Intends! I now (not (HoldsAt (dead P2) 6)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "Cogito Ergo Sum"
 :description "A formaliztion of Descartes' 'Cogito, Ergo Sum'"
 :assumptions {S1                  (Believes! I t1 (forall [x] (or (Name x) (Thing x))))
               S2                  (Believes! I t1 (forall (x) (iff (Name x) (not (Thing x)))))
               S3                  (Believes! I t1 (forall (x) (if (Thing x) (or (Real x) (Fictional x)))))
               S4                  (Believes! I t1 (forall (x) (if (Thing x) (iff (Real x) (not (Fictional x))))))
               A1                  (Believes! I t1 (forall (x) (if (Name x) (Thing (* x)))))
               A2                  (Believes! I t1
                                              (forall (y)
                                                      (if (Name y)
                                                        (iff (DeReExists y)
                                                             (exists x (and (Real x) (= x (* y))))))))
               Suppose             (Believes! I t1 (not (DeReExists I)))


               given               (Believes! I t1 (Name I))

               ;;;
               Perceive-the-belief (Believes! I t1 (Perceives! I t2 (Believes! I t3 (not (DeReExists I)))))
               If_P_B (Believes!
                       I t1
                       (forall [?agent]
                               (if (Perceives! I t2  (Believes! ?agent t3 (not (DeReExists ?agent))))
                                 (Real (* ?agent)))))}
 :goal        (and (Believes! I t4 (not (Real (* I))))
                   (Believes! I t4 (Real (* I))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Deep knowledge of physics"
 :description "Agent a knows that Atriya knows that Gergely knows Theorem NEAT"
 :assumptions {AxPh
               (Knows! a
                       (Knows! Atriya
                               (Knows! Gergely
                                       (forall [m]
                                               (forall [x y]
                                                       (if (IOb m)
                                                         (iff (exists p (and (Ph p) (W m p x) (W m p y)))
                                                              (= (speed x y) cm))))))))
               From-AxFd
               (Knows! a
                       (Knows! Atriya
                               (Knows! Gergely
                                       (forall [m]
                                               (forall [x y]
                                                       (if (not (= x y))
                                                         (exists z
                                                                 (and (= (speed x z) cm)
                                                                      (not (= (speed z y) cm))))))))))
               Definition-Event-P
               (Knows! a
                       (Knows! Atriya
                               (Knows! Gergely
                                       (forall [m b p]
                                               (iff (in b (ev m p))
                                                    (W m b p))))))}

 :goal        (Knows! a
                      (Knows! Atriya
                              (Knows! Gergely
                                      (forall [m x y]
                                              (if (IOb m)
                                                (if (not (= x y))
                                                  (not (= (ev m x) (ev m y)))))))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        ""
 :description ""
 :assumptions {1  (Knows! I now (forall [?x] (if (Agent ?x) (or (= ?x I) (= ?x P1) (= ?x P2) (= ?x P3)))))
               2  (Knows! I now (= 1 (nu alpha I P1 now)))
               3  (Knows! I now (= 1 (nu alpha I P2 now)))
               4  (Knows! I now (= 1 (nu alpha I P3 now)))
               5  (Knows! I now (= (- 1) (nu alpha I I now)))
               6  (Knows! I now (= alpha (Drop (self I) track1 3)))
               7  (Knows! I now (forall [a b] (if (= (self a) (self b)) (= a b))))
               8  (Knows! I now (forall [a] (= a (self a))))
               9  (Knows! I now
                          (and (not (= P1 P2)) (not (= P1 P3)) (not (= P1 I)) (not (= P2 P3)) (not (= P2 I)) (not (= P3 I))))
               10 (Knows! I now (and (>> 1 0) (<< (- 1) 0)))}
 :goal        (Knows! I now
                      (and (<< (nu (Drop (self I) track1 3) I I now) 0)
                           (forall [?agent]
                                   (if (Agent ?agent)
                                     (if (not (= ?agent (self I)))
                                       (>> (nu (Drop (self I) track1 3) I ?agent now) 0))))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "The Purloined Letter"
 :description "Dupin's reasoning as he goes through the case"

 :assumptions {1 (Believes! g t1 (hide m elaborate))
               2 (Knows! d  t1 (exists ?method (and (hide m ?method)
                                                    (or (= ?method plain)
                                                        (= ?method elaborate)))))
               3 (Believes! m t1 (Believes! g t2  (hide m elaborate)))
               4 (if (Believes! t1 m (Believes! g t2 (hide m elaborate))) (hide m plain))
               5 (if (Believes! m t1 (Believes! g t2 (hide m plain))) (hide m elaborate))
               6 (Believes! m (Believes! g (hide m elaborate)))
               7 (Believes! d t1 (if (Believes! m t2 (Believes! g t3 (hide m elaborate))) (hide m plain)))
               8 (Believes! d t1 (if (Believes! m t2 (Believes! g t3 (hide m plain))) (hide m elaborate)))
               9 (Believes! d t1 (Believes! m  t2 (Believes! g t3 (hide m elaborate))))
               }

 :goal        (Believes! d t4 (hide m plain))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "*cognitive-calculus-completeness-test-5*"
 :description "dt5"
 :assumptions {1 (Knows! a1 t1 (if H (and E D)))
               2 (Knows! a1 t1 (Knows! a2 t2 (if (or E My) R)))
               3 (Knows! a1 t1 (Knows! a2 t2 (Knows! a3 t2 (if Ma (not R)))))}
 :goal        (if H (not Ma))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "CIR-EXAMPLE-1"
 :description "Moe knows that engineer 1 knows that if a customer
               has complained about a thing, then that thing is not read"
 :assumptions {1 (Knows! Moe t1
                         (Knows! engineer1 t1
                                 (forall [?thing]
                                         (if (CustomerComplained ?thing)
                                           (and (needs ?thing tested) (needs ?thing certified))))))
               2 (Knows! Moe t1
                         (Knows! engineer1 t2
                                 (forall [?thing]
                                         (if (or (needs ?thing tested)
                                                 (needs ?thing cleaned))
                                           (request-more-time ?thing)))))

               3 (Knows! Moe t1
                         (Knows! engineer1 t2
                                 (Knows! engineer2 t2
                                         (forall [?thing]
                                                 (if (complete ?thing) (not (request-more-time ?thing)))))))}
 :goal        (Knows! Moe t2
                      (Knows! engineer1 t2
                              (if (CustomerComplained thing) (not (complete thing)))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "DDE Base Counterfactual C5a"
 :description "Deriving C5a from a more general desire axiom that is common knowledge.
               The main goal here is to show that a reasonable representation of C5a is possible. "
 :assumptions {;; It is common knowledge that if if one knows that one is in a dilemma, then one desires to be in a
              ;; non-dilemma situation
                Common-Knowledge-General-Desire-Axiom
                (Common! now
                         (Knows! I now
                                 (if
                                   (exists [?dilemma-situation]
                                           (and (HoldsAt (in I ?dilemma-situation) now)
                                                (forall [?alpha]
                                                        (if (Action I ?alpha ?dilemma-situation now)
                                                          (exists [?fluent]
                                                                  (or (and (negative ?fluent) (Initiates (action I ?alpha) ?fluent now))
                                                                      (and (positive ?fluent) (Terminates (action I ?alpha) ?fluent now))))))))
                                   (Desires! I now
                                             (exists [?situation]
                                                     (and
                                                      (HoldsAt (in I ?situation) now)
                                                      (exists [?alpha]
                                                              (and (Action I ?alpha ?situation now)
                                                                   (not (Ought! I now ?situation (not (Happens (action I ?alpha) now))))
                                                                   (not
                                                                     (exists [?fluent]
                                                                             (or (and (negative ?fluent) (Initiates (action I ?alpha) ?fluent now))
                                                                                 (and (positive ?fluent) (Terminates (action I ?alpha) ?fluent now)))))))))))))

                ;;; I know I am in the trolley situation
                Current-situation
                (Knows! I now (HoldsAt (in I trolley-situation) now))

                ;;; All sanctioned actions in the trolley situation are bad.
                Knowledge-about-the-trolly-situation
                (Knows! I now
                        (forall [?alpha]
                                (if (Action I ?alpha trolley-situation now)
                                  (exists [?fluent]
                                          (or (and (negative ?fluent) (Initiates (action I ?alpha) ?fluent now))
                                              (and (positive ?fluent) (Terminates (action I ?alpha) ?fluent now)))))))}

 :goal        (and
               (Believes! I now (HoldsAt (in I trolley-situation) now))
               (Desires! I now
                         (exists [?situation]
                                 (and
                                  (HoldsAt (in I ?situation) now)
                                  (exists [?alpha]
                                          (and (Action I ?alpha ?situation now)
                                               (not (Ought! I now ?situation (not (Happens (action I ?alpha) now))))
                                               (not
                                                 (exists [?fluent]
                                                         (or (and (negative ?fluent) (Initiates (action I ?alpha) ?fluent now))
                                                             (and (positive ?fluent) (Terminates (action I ?alpha) ?fluent now)))))))))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name        "DDE Base Counterfactual C5b"
 :description "Deriving C5b from more general axioms.
               The main goal here is to show that a reasonable representation of C5b is possible. "

 :assumptions {;; It is common knowledge that in a non-dilemma, one will not perform a bad action.
                Common-Knowledge-Non-Dilemma
                (Common! now
                         (if
                           (exists [?situation]
                                   (and
                                    (HoldsAt (in I ?situation) now)
                                    (exists [?alpha]
                                            (and (Action I ?alpha ?situation now)
                                                 (not (Ought! I now ?situation (not (Happens (action I ?alpha) now))))
                                                 (not
                                                   (exists [?fluent]
                                                           (or (and (negative ?fluent) (Initiates (action I ?alpha) ?fluent now))
                                                               (and (positive ?fluent) (Terminates (action I ?alpha) ?fluent now)))))))))
                           (forall [?aT]
                                   (if (happens (action I ?aT) now)
                                     (not
                                       (exists [?fluent]
                                               (or (and (negative ?fluent) (Initiates (action I ?aT) ?fluent) now)
                                                   (and (positive ?fluent) (Terminates (action I ?aT) ?fluent) now))))))))

                DDEAction
                (Knows! I now
                        (exists [?fluent]
                                (or (and (negative ?fluent) (Initiates (action I aD) ?fluent) now)
                                    (and (positive ?fluent) (Terminates (action I aD) ?fluent) now))))}

 :goal        (Believes! I now
                         (=>
                           (exists [?situation]
                                   (and
                                    (HoldsAt (in I ?situation) now)
                                    (exists [?alpha]
                                            (and (Action I ?alpha ?situation now)
                                                 (not (Ought! I now ?situation (not (Happens (action I ?alpha) now))))
                                                 (not
                                                   (exists [?fluent]
                                                           (or (and (negative ?fluent) (Initiates (action I ?alpha) ?fluent now))
                                                               (and (positive ?fluent) (Terminates (action I ?alpha) ?fluent now)))))))))
                           (not (happens (action I aD) now))))}


{:name        "Self Sacrifice"
 :description ""
 :assumptions {A1  (Knows! I now (forall [?x] (if (Agent ?x) (or (= ?x I) (= ?x P1) (= ?x P2) (= ?x P3)))))
               A2  (Knows! I now (= 1 (nu alpha I P1 now)))
               A3  (Knows! I now (= 1 (nu alpha I P2 now)))
               A4  (Knows! I now (= 1 (nu alpha I P3 now)))
               A5  (Knows! I now (= (- 1) (nu alpha I I now)))
               A6  (Knows! I now (= alpha (Drop (self I) track1 3)))
               A7  (Common! (forall [a b] (if (= (self a) (self b)) (= a b))))
               A8  (Knows! I now (forall [a] (= a (self a))))
               A9  (Knows! I now
                           (and (not (= P1 P2)) (not (= P1 P3)) (not (= P1 I)) (not (= P2 P3)) (not (= P2 I)) (not (= P3 I))))
               A10 (Knows! I now (and (>> 1 0) (<< -1 0)))}

 :goal        (Knows! I now
                      (forall [?agent]
                              (if (Agent ?agent)
                                (if (not (= ?agent (self I))) (>> (nu alpha I ?agent now) 0)))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:assumptions
 {A1 (Perceives! student t1 (Believes! (embodiment a) t1 (Holds (Prop watch stopped) t1)))
  A2 (Perceives! student t2 (Believes! (embodiment b) t2 (Holds (Prop watch stopped) t2)))


  A3 (Believes! student t3 (PersonalObject watch))
  A4 (Believes! student t4
                (if (exists [?agent1 ?agent2 ?u ?prop ?time1 ?time2]
                            (and (PersonalObject ?u)
                                 (Believes! ?agent1 ?time1 (Holds (Prop ?u ?prop) ?time1))
                                 (Believes! ?agent2 ?time2 (Holds (Prop ?u ?prop) ?time2))))
                  (= (identityOf ?agent1) (identityOf ?agent2))))}


 :goal (Believes! student t5 (= (identityOf (embodiment a)) (identityOf (embodiment b))))}


{:name        "Counterfactual 1"
 :description ""
 :assumptions {A1 (forall [?x]
                          (if (GoToDoctor ?x) (not (Sick ?x))))
               A2 (not (GoToDoctor john))}

 :goal        (=> (GoToDoctor john) (not (Sick john)))}

{:name        "Counterfactual 2"
 :description ""
 :assumptions {A1 (forall [?x]
                          (if (GoToDoctor ?x) (not (Sick ?x))))
               A2 (not (GoToDoctor john))}

 :goal        (if (GoToDoctor john) (and P (not P)))}


{:name        "Counterfactual Mortality 1"
 :description ""
 :assumptions {A1 (forall [?x] (if (Human ?x) (Mortal ?x)))
               A2 (Human socrates)}

 :goal        (=> (not (Mortal socrates)) (not (Human socrates)))}


{:name        "Counterfactual Mortality 2"
 :description ""
 :assumptions {A1 (forall [?x] (if (Human ?x) (Mortal ?x)))
               A2 (Human socrates)}

 :goal        (if (not (Mortal socrates)) (and P (not P)))}


{:name        "Counterfactual Identity A  1"
 :description ""
 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))}

 :goal        (=> (= jack jim) (CanAffordLuxury jim))}


{:name        "Counterfactual Identity A 2"
 :description ""
 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))}

 :goal        (if (= jack jim) (and P (not P)))}


{:name        "Counterfactual Identity B  1"
 :description ""
 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))
               A4 (not (= jim jack))}

 :goal        (=> (= jack jim) (CanAffordLuxury jim))}


{:name        "Counterfactual Identity B 2"
 :description ""
 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))
               A4 (not (= jim jack))}

 :goal        (if (= jack jim) (and P (not P)))}


{:name        "Counterfactual Disjunction 1"
 :description ""
 :assumptions {A1 (forall [?x] (or (Big ?x) (Small ?x)))
               A2 (Big tree)}

 :goal        (=> (not (Big tree)) (Small tree))}


{:name        "Counterfactual Disjunction 2"
 :description ""
 :assumptions {A1 (forall [?x] (or (Big ?x) (Small ?x)))
               A2 (Big tree)}

 :goal        (if (not (Big tree)) (and P (not P)))}


{:name        "Counterfactual Disjunction A 1"
 :description ""
 :assumptions {A1 (forall [?x] (or (Human ?x) (Animal ?x)))
               A2 (Human j)
               A3 (forall [?x] (if (Human ?x) (Thinks ?x)))}

 :goal        (=> (not (Thinks j))
                  (or (Animal j)
                      (exists [?x] (and (Human ?x) (not (Thinks ?x))))))}


{:name        "Counterfactual Disjunction A 2"
 :description ""
 :assumptions {A1 (forall [?x] (or (Human ?x) (Animal ?x)))
               A2 (Human j)
               A3 (forall [?x] (if (Human ?x) (Thinks ?x)))}

 :goal        (if (not (Thinks j)) (and P (not P)))}


{:name        "Counterfactual Linking A 1"
 :description ""
 :assumptions {A1 (if P Q)
               A2 (if Q R)
               A3 (if R S)
               A4 (not P)}

 :goal        (=> P R)}


{:name        "Counterfactual Linking A 2"
 :description ""
 :assumptions {A1 (if P Q)
               A2 (if Q R)
               A3 (if R S)
               A4 (not P)}

 :goal        (if P (and P (not P)))}


{:name        "Counterfactual Reverse Linking A 1"
 :description ""
 :assumptions {A1 (if P Q)
               A2 (if Q R)
               A3 (if R S)
               A4 S}

 :goal        (=> (not S) (not P))}


{:name        "Counterfactual Reverse Linking A 2"
 :description ""
 :assumptions {A1 (if P Q)
               A2 (if Q R)
               A3 (if R S)
               A4 S}

 :goal        (if (not S) (and P (not P)))}


{:name        "Counterfactual Reverse Linking B 1"
 :description ""
 :assumptions {A1 (if (forall [?x] (P ?x)) Q)
               A2 (if Q R)
               A3 (if R S)
               A4 S}

 :goal        (=> (not S) (exists [?x] (not (P ?x))))}


{:name        "Counterfactual Reverse Linking B 2"
 :description ""
 :assumptions {A1 (if (forall [?x] (P ?x)) Q)
               A2 (if Q R)
               A3 (if R S)
               A4 S}

 :goal        (if (not S) (and P (not P)))}


{:name        "Counterfactual 1"
 :description ""
 :assumptions {A1 (forall [?x]
                          (if (GoToDoctor ?x) (not (Sick ?x))))
               A2 (not (GoToDoctor john))}

 :goal        (=> (GoToDoctor john) (not (Sick john)))}

{:name        "Counterfactual 2"
 :description ""
 :assumptions {A1 (forall [?x]
                          (if (GoToDoctor ?x) (not (Sick ?x))))
               A2 (not (GoToDoctor john))}

 :goal        (if (GoToDoctor john) Z)}


{:name        "Counterfactual modal 1"
 :description ""
 :assumptions {A1 (Believes! a
                             (forall [?x]
                                     (if (GoToDoctor ?x) (not (Sick ?x)))))
               A2 (Believes! a (not (GoToDoctor john)))}

 :goal        (Believes! a (=> (GoToDoctor john) (not (Sick john))))}


{:name        "Counterfactual modal 2"
 :description ""
 :assumptions {A1 (Believes! a
                             (forall [?x]
                                     (if (GoToDoctor ?x) (not (Sick ?x)))))
               A2 (Believes! a (not (GoToDoctor john)))}

 :goal        (Believes! a (if (GoToDoctor john) (and P (not P))))}


{:name        "Counterfactual Mortality 1"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (if (Human ?x) (Mortal ?x))))
               A2 (Believes! a now (Human socrates))}

 :goal        (Believes! a now (=> (not (Mortal socrates)) (not (Human socrates))))}


{:name        "Counterfactual Mortality 2"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (if (Human ?x) (Mortal ?x))))
               A2 (Believes! a now (Human socrates))}

 :goal        (Believes! a now (if (not (Mortal socrates)) (and P (not P))))}


{:name        "Counterfactual Identity A  1"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x))))
               A2 (Believes! a now (Rich jack))
               A3 (Believes! a now (not (Rich jim)))}

 :goal        (Believes! a now (=> (= jack jim) (CanAffordLuxury jim)))}


{:name        "Counterfactual Identity A 2"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x))))
               A2 (Believes! a now (Rich jack))
               A3 (Believes! a now (not (Rich jim)))}

 :goal        (Believes! a now (if (= jack jim) (and P (not P))))}


{:name        "Counterfactual Disjunction 1"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (or (Big ?x) (Small ?x))))
               A2 (Believes! a now (Big tree))}

 :goal        (Believes! a now (=> (not (Big tree)) (Small tree)))}


{:name        "Counterfactual Disjunction 2"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (or (Big ?x) (Small ?x))))
               A2 (Believes! a now (Big tree))}

 :goal        (Believes! a now (if (not (Big tree)) (and P (not P))))}


{:name        "Counterfactual Disjunction A 1"
 :description ""
 :assumptions {A1 (Believes! a now (Believes! b now (forall [?x] (or (Big ?x) (Small ?x)))))
               A2 (Believes! a now (Believes! b now (Big tree)))}

 :goal        (Believes! a now (Believes! b now (=> (not (Big tree)) (Small tree))))}


{:name        "Counterfactual Disjunction A 2"
 :description ""
 :assumptions {A1 (Believes! a now (Believes! b now (forall [?x] (or (Big ?x) (Small ?x)))))
               A2 (Believes! a now (Believes! b now (Big tree)))}

 :goal        (Believes! a now (Believes! b now (if (not (Big tree)) (and P (not P)))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Disjunction Ab 1"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (or (Human ?x) (Animal ?x))))
               A2 (Believes! a now (Human j))
               A3 (Believes! a now (forall [?x] (if (Human ?x) (Thinks ?x))))}

 :goal        (Believes! a now
                         (=> (not (Thinks j))
                             (or (Animal j)
                                 (exists [?x] (and (Human ?x) (not (Thinks ?x)))))))}


{:name        "Counterfactual Disjunction Ab 2"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (or (Human ?x) (Animal ?x))))
               A2 (Believes! a now (Human j))
               A3 (Believes! a now (forall [?x] (if (Human ?x) (Thinks ?x))))}

 :goal        (Believes! a now (if (not (Thinks j)) (and P (not P))))}



{:name        "Holmes and Watson"
 :description ""
 :assumptions {Premise1 (Knows! holmes
                                (if (Believes! watson
                                               (and (not (Knows! holmes t1 (PersonalFact (inMilitary watson))))
                                                    (Knows! holmes t2 (PersonalFact (inMilitary watson)))))
                                  (Believes! watson (not (Amateur holmes)))))

               Premise2 (Knows! holmes (Knows! watson (not (Knows! holmes t1 (PersonalFact (inMilitary watson))))))

               Premise3 (Knows! holmes
                                (Believes! watson
                                           (Knows! holmes t2
                                                   (if (and (tan watson) (wounded watson))
                                                     (PersonalFact (inMilitary watson))))))
               Premise4 (Knows! holmes
                                (Believes! watson
                                           (Knows! holmes t2
                                                   (and (tan watson) (wounded watson)))))
               }


 :goal        (Knows! holmes (Believes! watson (not (Amateur holmes))))


 }

{
  :description "Forward inference within modal formulae"

  :assumptions {
                 1 (Knows! ua now (LessUnethical emptyroad fieldwithpowerlines))
                 2 (Knows! ua now (LessUnethical fieldwithpowerlines fieldwithpeople))

                 3 (Knows! ua now (forall [?x ?y ?z] (implies (and (LessUnethical ?x ?y) (LessUnethical ?y ?z))
                                                              (LessUnethical ?x ?z))))

                 4 (Knows! ua now (if (and (LessUnethical emptyroad fieldwithpowerlines)
                                           (LessUnethical emptyroad fieldwithpeople))
                                    (BestOption emptyroad)))

                 6 (Knows! ua now (implies (BestOption emptyroad)
                                           (Ought! ua now crisis (happens (action ua (Land emptyroad)) now))))

                 7 (Believes! ua now crisis)
                 }
  :goal (Intends! ua now  (happens (action ua (Land emptyroad)) now))
  }