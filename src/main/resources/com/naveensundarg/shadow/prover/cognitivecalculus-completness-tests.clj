
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
 :goal         (and (Knows! a1 t1 P) (Knows! a2 t1 P))

 }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "conjunction-test-2"
 :description "Conjunctions"
 :assumptions {1 (and (Common! t0 P) (Common! t0 Q) ) }
 :goal         (Knows! a1 t1 (and P Q))

 }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "conjunction-test-3"
 :description "Conjunctions"
 :assumptions {1 (and (Common! t0 (forall (?x) (if (human ?x) (mortal ?x))))
                      (Common! t0 (human socrates)))
               }
 :goal         (Knows! a1 t1 (mortal socrates))

 }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-3*"
 :description "Bird Theorem and Jack"
 :assumptions {1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y))))
                   (Knows! jack t0 BirdTheorem))}
 :goal        (Knows! jack t0 BirdTheorem)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-4*"
 :description "Bird Theorem"
 :assumptions {1 (Knows! a1 t0 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BIRDTHEOREM))}
 :goal        BIRDTHEOREM}

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

{:name        "*cognitive-calculus-completeness-test-11*"
 :description "dt8.a"
 :assumptions {1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) (Knows! a now Q))}
 :goal        Q}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-12*"
 :description "dt10"
 :assumptions {1 (or (Knows! a now P) (Knows! b now P))}
 :goal        P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-13*"
 :description "dt11"
 :assumptions {1 (Believes! a now P)
               2 (Believes! a now (if P (Knows! b now Q)))}
 :goal        (Believes! a now (Knows! b now Q))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-14*"
 :description "dt11.a"
 :assumptions {1 (if P (Knows! b now (and (Knows! c t1 Q1) (Knows! c t2 Q2))))
               2 (or (Knows! a now P) (Knows! b now P))}
 :goal        (and Q1 Q2)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-15*"
 :description "dt12 Lemma 6 from http://kryten.mm.rpi.edu/arkoudas.bringsjord.clima.crc.pdf"
 :assumptions {1 (not (Knows! a now P))
               2 (if (not Q) (Knows! a now (not Q)))
               3 (Knows! a now (if (not Q) P))}
 :goal        Q}

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

{:name        "*cognitive-calculus-completeness-test-18*"
 :description "wise man puzzle n =2"
 :assumptions {1 (Common! now (iff (not (Marked a2)) (Marked a1)))
               2 (Common! now (if (not (Marked a2)) (Knows! a1 now (not (Marked a2)))))
               3 (Common! now (not (Knows! a1 now (Marked a1))))}
 :goal        (and (Marked a2) (not (Marked a1)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
               2 (Believes! jack t0 (Ought! jack t0 P (happens (action jack A) t0))) }
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
 :assumptions {1 (Common! t0 (forall (?x) (if (cries ?x) (injured ?x))) )
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
 :assumptions {1 (Believes!  robot t1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BirdTtheorem))
               }
 :goal        (Believes! robot t2 BirdTtheorem)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-4*"
 :description "Testing closure"
 :assumptions {1 (Believes!  robot1 t1  (Believes! robot2 t1 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BirdTtheorem)))
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
 :goal        (Believes! robot1 t2 (happens (action robot2 (help mary)) t2)) }




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-closure-test-6*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! t0 (forall (?x) (if (and (honest ?x) (criesForHelp ?x)) (injured ?x))))
               ;; It is commonly known that if someone is honest and cries for help, then they are injured.

               2 (Common! t1 (Ought! robot2 t2 (injured mary) (happens (action robot2 (help mary)) t2)))

               3 (Believes! robot1 t0 (Believes! robot2 t0 (honest mary)))

               4 (Common! t1 (criesForHelp mary))


               }
 :goal        (Believes! robot1 t2 (happens (action robot2 (help mary)) t2)) }



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*licato*"
 :description "from licato's paper"
 :assumptions {1  (Knows! a t (or (isExit A) (isExit B)))

               2 (Perceives! a t (not (isExit A)))
               }
 :goal        (Knows! a t (isExit B)) }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{
 :name "universal intro inside a knows"
 :description ""
 :assumptions {
               1 (forall (?x) (if (P ?x) (Knows! ?x U)) )
               2 (P a)
               }

 :goal (Knows! a U)

 }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "false-belief-task-M1"
 :description "Method 1 in the False Belief Task Paper"
 :assumptions {1 (Perceives! a1 (happens (action a2 alpha) t))
               2 (Common! (forall (?a ?alpha ?t) (if (happens (action ?a ?alpha) ?t)
                                                   (Knows! ?a  (happens (action ?a ?alpha) ?t)))))}

 :goal (Knows! a1 (Knows! a2 (happens (action a2 alpha) t)))}






{:name "false-belief-task-M2"
 :description "Method 2 in the False Belief Task Paper"
 :assumptions {P1 (Common! (forall (?t) (initiates e f ?t)))
               P2 (Knows! a1 (Knows! a2 (happens e t1)))
               P3 (Common! (< t1 t2))
               P4 (Knows! a1 (Knows! a2 (not (exists (?e ?t) (and (happens ?e ?t) (< t1 ?t) (< ?t t2) (terminates ?e f ?t))))))
               A2 (Common! (forall (?e ?f ?t1 ?t2)
                                   (if  (and (happens ?e ?t1) (initiates ?e ?f ?t1) (< ?t1 ?t2) (not (clipped ?t1 ?f ?t2)))
                                     (holds ?f ?t2))))
               A3 (Common! (forall (?t1 ?f ?t2)
                                   (iff (clipped ?t1 ?f ?t2)
                                        (exists (?e ?t)
                                                (and (happens ?e ?t)
                                                     (< ?t1 ?t)
                                                     (< ?t ?t2)
                                                     (terminates ?e ?f ?t))))))
               }

 :goal  (Knows! a1 (Knows! a2 (holds f t2))) }


