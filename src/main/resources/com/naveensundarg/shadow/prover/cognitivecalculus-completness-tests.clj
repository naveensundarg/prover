;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-1*"
 :description "kicking the tires"
 :sorts [Agent Time Proposition]
 :definitions [(a1 Agent)
               (t1 Time)
               (P Proposition)]
 :assumptions {1 (Knows! a1 t1 P)}
 :goal        P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*cognitive-calculus-completeness-test-2*"
 :description "kicking the tires"
 :assumptions {1 (Believes! a1 t0 P)
               2 (Believes! a1 t0 (if P Q))}
 :goal        (Believes! a1 t0 Q)}

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