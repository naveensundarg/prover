;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-1*"
 :description "kicking the tires"
 :assumptions {1 (Knows! a1 t1 P)}
 :goal        P}

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
 :goal        (and (Knows! a1 t1 P) (Knows! a2 t1 P))

 }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "conjunction-test-2"
 :description "Conjunctions"
 :assumptions {1 (and (Common! t0 P) (Common! t0 Q))}
 :goal        (Knows! a1 t1 (and P Q))

 }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "conjunction-test-3"
 :description "Conjunctions"
 :assumptions {1 (and (Common! t0 (forall (?x) (if (human ?x) (mortal ?x))))
                      (Common! t0 (human socrates)))
               }
 :goal        (Knows! a1 t1 (mortal socrates))

 }

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;{:name        "*cognitive-calculus-completeness-test-4*"
; :description "Bird Theorem"
; :assumptions {1 (Knows! a1 t0 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BIRDTHEOREM))}
; :goal        BIRDTHEOREM}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
 :assumptions {1 (if P (Knows! jack now (not (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))))))}
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
               3 (Believes! robot now (Ought! robot now (injured soldier) (happens (action robot (help soldier)) now)))
               }
 :goal        (happens (action robot (help soldier)) now)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-ought-injured-test-3*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! now (if (cries soldier) (injured soldier)))
               2 (Knows! robot now (cries soldier))
               3 (Common! now (Ought! robot now (injured soldier) (happens (action robot (help soldier)) now)))
               }
 :goal        (happens (action robot (help soldier)) now)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-ought-injured-test-3-complex*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! t0 (forall (?x) (if (cries ?x) (injured ?x))))
               2 (Knows! robot t1 (cries soldier))
               3 (Common! t2 (Ought! robot t2 (injured soldier) (happens (action robot (help soldier)) t2)))
               }
 :goal        (happens (action robot (help soldier)) t2)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "*cognitive-calculus-ought-injured-test-4*"
 :description "Testing the ought rule"
 :assumptions {1 (Believes! robot now INJURED)
               2 (Common! now (Ought! robot now INJURED (happens (action robot HELP) now)))
               }
 :goal        (happens (action robot HELP) now)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-ought-injured-test-4*"
 :description "Testing the ought rule"
 :assumptions {1 (Believes! robot now INJURED)
               2 (Common! now (Ought! robot now INJURED (happens (action robot HELP) now)))
               }
 :goal        (happens (action robot HELP) now)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-1*"
 :description "Testing closure"
 :assumptions {1 (Believes! robot t1 (forall (?x) (if (Man ?x) (Mortal ?x))))
               2 (Believes! robot t2 (Man socrates))
               }
 :goal        (Believes! robot t3 (Mortal socrates))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-2*"
 :description "Testing closure"
 :assumptions {1 (Common! t1 (forall (?x) (if (Man ?x) (Mortal ?x))))
               2 (Common! t2 (Man socrates))
               }
 :goal        (Believes! robot t1 (forall (?x) (if (Man ?x) (Mortal ?x))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-3*"
 :description "Testing closure"
 :assumptions {1 (Believes! robot t1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BirdTtheorem))
               }
 :goal        (Believes! robot t2 BirdTtheorem)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-4*"
 :description "Testing closure"
 :assumptions {1 (Believes! robot1 t1 (Believes! robot2 t1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BirdTtheorem)))
               }
 :goal        (Believes! robot1 t2 (Believes! robot2 t1 BirdTtheorem))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-5*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! t0 (forall (?x) (if (and (honest ?x) (criesForHelp ?x)) (injured ?x))))
               ;; It is commonly known that if someone is honest and cries for help, then they are injured.

               2 (Common! t1 (Ought! robot2 t2 (injured mary) (happens (action robot2 (help mary)) t2)))

               3 (Believes! robot1 t0 (Believes! robot2 t0 (honest mary)))

               4 (Common! t1 (criesForHelp mary))
               }
 :goal        (Believes! robot1 t2 (happens (action robot2 (help mary)) t2))}




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-6*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! t0 (forall (?x) (if (and (honest ?x) (criesForHelp ?x)) (injured ?x))))
               ;; It is commonly known that if someone is honest and cries for help, then they are injured.

               2 (Common! t1 (Ought! robot2 t2 (injured mary) (happens (action robot2 (help mary)) t2)))

               3 (Believes! robot1 t0 (Believes! robot2 t0 (honest mary)))

               4 (Common! t1 (criesForHelp mary))

               }
 :goal        (Believes! robot1 t2 (happens (action robot2 (help mary)) t2))}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*licato*"
 :description "from licato's paper"
 :assumptions {1 (Knows! a t (or (isExit A) (isExit B)))

               2 (Perceives! a t (not (isExit A)))
               }
 :goal        (Knows! a t (isExit B))}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{
 :name        "universal intro inside a knows"
 :description ""
 :assumptions {
               1 (forall (?x) (if (P ?x) (Knows! ?x U)))
               2 (P a)
               }

 :goal        (Knows! a U)

 }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "false-belief-task-M1"
 :description "Method 1 in the False Belief Task Paper"
 :assumptions {1 (Perceives! a1 (happens (action a2 alpha) t))
               2 (Common! (forall (?a ?alpha ?t) (if (happens (action ?a ?alpha) ?t)
                                                   (Knows! ?a (happens (action ?a ?alpha) ?t)))))}

 :goal        (Knows! a1 (Knows! a2 (happens (action a2 alpha) t)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "DDE base"
 :description "DDE"
 :assumptions {
               I2
               (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P1) ?t)))) )

               I3
               (Knows! I now situation)

               I4
               (Believes! I now (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P1) ?t)))) ))}

 :goal        (Intends! I now (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P1) ?t)))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



{:name        "DDE 3a 1"
 :description "DDE"
 :assumptions {
               I2
               (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P2) ?t)))) )

               I3
               (Knows! I now situation)

               I4
               (Believes! I now (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                                             (not (exists [?t] (HoldsAt (dead P2) ?t)))) ))}

 :goal        (Intends! I now (not (HoldsAt (dead P1) 5)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "DDE 3a 2"
 :description "DDE"
 :assumptions {
               I2
               (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                            (not (exists [?t] (HoldsAt (dead P2) ?t)))) )

               I3
               (Knows! I now situation)

               I4
               (Believes! I now (Ought! I now situation (and (not (exists [?t] (HoldsAt (dead P1) ?t)))
                                                             (not (exists [?t] (HoldsAt (dead P2) ?t)))) ))}

 :goal        (Intends! I now (not (HoldsAt (dead P2) 6)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Cogito Ergo Sum"
 :description "A formaliztion of Descartes' Cogito Ergo Sum"
 :assumptions {

               S1 (Believes! I (forall [x] (or (Name x) (Thing x))))
               S2 (Believes! I (forall (x) (iff (Name x) (not (Thing x)))) )
               S3 (Believes! I (forall (x) (if (Thing x) (or (Real x) (Fictional x)))))
               S4 (Believes! I (forall (x) (if (Thing x) (iff (Real x) (not (Fictional x))))))


               ;;;
               A1 (Believes! I (forall (x) (if (Name x) (Thing (* x)))))


               A2 (Believes! I (forall (y) (if (Name y)
                                             (iff  (DeReExists y)
                                                   (exists x (and (Real x) (= x (* y))))))))

               ;;;
               ;

               Suppose (Believes! I (not (DeReExists I)))



               given (Believes! I (Name I))

               ;;;
               Perceive-the-belief (Believes! I (Perceives! I (Believes! I (not (DeReExists I)))))
               If_P_B

               (Believes!
                        I
                        (forall [?agent]
                                (if (Perceives! I (Believes! ?agent (not (DeReExists ?agent))))
                                  (Real (* ?agent)))))



               }
 :goal        (and (Believes! I  (not (Real (* I))))
                   (Believes! I  (Real (* I)) ))

 }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Deep knowledge of physics"
 :description "Agent a knows that Atriya knows that Gergely knows Theorem NEAT"
 :assumptions {
               AxPh
               (Knows! a
                       (Knows! Atriya
                               (Knows! Gergely
                                      (forall [m] (forall [x y]
                                                        (if (IOb m)
                                                          (iff (exists p (and (Ph p) (W m p x) (W m p y)))
                                                               (= (speed x y) cm))))))))
               From-AxFd
               (Knows! a
                       (Knows! Atriya
                               (Knows! Gergely
                                       (forall [m] (forall [x y]
                                                         (if (not (= x y))
                                                           (exists z (and (= (speed x z) cm)
                                                                          (not (= (speed z y) cm))))))))))
               Definition-Event-P
               (Knows! a
                       (Knows! Atriya
                               (Knows! Gergely
                                        (forall [m b p]
                                                (iff (in b (ev m p))
                                                     (W m b p))))))

               }

 :goal        (Knows! a
                       (Knows! Atriya
                               (Knows! Gergely
                                        (forall [m x y] (if (IOb m) (if (not (= x y))
                                                                      (not (= (ev m x) (ev m y)))))))))

 }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



{:name        "The Purloined Letter"
 :description "Dupin's reasoning as he goes through the case"

 :assumptions {
               1 (Believes! g (hide m elaborate))
               2 (Believes! d (or (hide m elaborate) (hide m plain)))
               3 (Believes! m (Believes! g (hide m elaborate)))
               4 (if (Believes! m (Believes! g (hide m elaborate))) (hide m plain))
               5 (if (Believes! m (Believes! g (hide m plain))) (hide m elaborate))
               6 (Believes! m (Believes! g (hide m elaborate)))
               7 (Believes! d (if (Believes! m (Believes! g (hide m elaborate))) (hide m plain)))
               8 (Believes! d (if (Believes! m (Believes! g (hide m plain))) (hide m elaborate)))
               9 (Believes! d (Believes! m (Believes! g (hide m elaborate))))}


 :goal (Believes! d (hide m plain))}

{:name ""
 :description ""
 :assumptions {1 (Knows! I now (forall [?x] (if (Agent ?x) (or (= ?x I) (= ?x P1) (= ?x P2) (= ?x P3)))))
               2 (Knows! I now (= 1 (nu alpha I P1 now)))
               3 (Knows! I now (= 1 (nu alpha I P2 now)))
               4 (Knows! I now (= 1 (nu alpha I P3 now)))
               5 (Knows! I now (= (- 1) (nu alpha I I now)))
               6 (Knows! I now (= alpha (Drop (self I) track1 3)))
               7 (Knows! I now (forall [a b] (if (= (self a) (self b)) (= a b))))
               8 (Knows! I now (forall [a] (= a (self a))))
               9 (Knows! I now (and (not (= P1 P2)) (not (= P1 P3)) (not (= P1 I)) (not (= P2 P3)) (not (= P2 I)) (not (= P3 I))))
               10 (Knows! I now (and (>> 1 0) (<< (- 1)  0)))
               }
 :goal (Knows! I now (and (<< (nu (Drop (self I) track1 3) I I now) 0)
                          (forall [?agent] (if (Agent ?agent) (if (not (= ?agent (self I))) (>> (nu (Drop (self I) track1 3) I ?agent now) 0)))))  )}