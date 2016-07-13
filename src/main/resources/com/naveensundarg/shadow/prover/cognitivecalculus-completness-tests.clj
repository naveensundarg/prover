"begin" "kicking the tires"
"assumption.1" (Knows! a1 1 P)
"goal" P
"end"

"begin" "kicking the tires"
"assumption" (Believes! a1 0 (if P Q))
"assumption" (Believes! a1 0 P)
"goal" (Believes! a1 0 Q)
"end"

"begin" "bird theorem and jack"
"assumption" (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) (Knows! jack 0 BirdTheorem))
"goal" (Knows! jack 0 BirdTheorem)
"end"

"begin" "bird theorem and jack"
"assumption" (Knows! a1 0 (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) BIRDTHEOREM))
"goal" BIRDTHEOREM
"end"

"begin" "dt5"
"assumption" (Knows! a1 t1 (if H (and E D)))
"assumption" (Knows! a1 t1 (Knows! a2 t2 (if (or E My) R)))
"assumption" (Knows! a1 t1 (Knows! a2 t2 (Knows! a3 t2 (if Ma (not R)))))
"goal" (if H (not Ma))
"end"

"begin" "dt6"
"assumption" (and P (Knows! a 0 Q))
"goal" Q
"end"

"begin" "dt6.a"
"assumption" (and P (Knows! a 0 Q))
"goal" (and P Q)
"end"

"begin" "dt6.a"
"assumption" (and P (Knows! a 0 Q))
"goal" (or P Q)
"end"

"begin" "dt7"
"assumption" (and P (Knows! a now (and Q (Knows! b now R1))))
"assumption" (and P (Knows! a now (and Q (Knows! b now R2))))
"goal" (and R1 R2)
"end"

"begin" "dt8"
"assumption" P
"assumption" (if P (Knows! a now Q))
"goal" Q
"end"

"begin" "dt8.a"
"assumption" (if (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))) (Knows! a now Q))
"goal" Q
"end"

"begin" "dt10"
"assumption" (or (Knows! a now P) (Knows! b now P))
"goal" P
"end"

"begin" "dt10.a"
"assumption" (Believes! a now (if P (Knows! b now Q)))
"assumption" (Believes! a now P)
"goal"(Believes! a now (Knows! b now Q))
"end"

"begin" "dt11"
"assumption" (or (Knows! a now P) (Knows! b now P))
"assumption" (if P (Knows! b now (and  (Knows! c t1 Q1) (Knows! c t2 Q2))))
"goal" (and Q1 Q2)
"end"

"begin" "dt12 Lemma 6 from http://kryten.mm.rpi.edu/arkoudas.bringsjord.clima.crc.pdf"
"assumption" (not (Knows! a now P))
"assumption" (Knows! a now (if (not Q) P))
"assumption" (if (not Q) (Knows! a now (not Q)))
"goal" Q
"end"

"begin" "dt15"
"assumption" (if P (Knows! jack now (not (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y)))))))
"goal" (not P)
"end"


"begin" "dt16 check DR1"
"assumption" (Common! now (Common! now P))
"goal" P
"end"


"begin" "wise man puzzle n =2"
"assumption" (Common! now (not (Knows! a1 now (Marked a1))))
"assumption" (Common! now (if (not (Marked a2)) (Marked a1)))
"assumption" (Common! now (if (not (Marked a1)) (Marked a2)))
"assumption" (Common! now (if (Marked a1) (not (Marked a2))))
"assumption" (Common! now (if (Marked a2) (not (Marked a1))))
"assumption" (Common! now (if (not (Marked a2)) (Knows! a1 now (not (Marked a2)))))
"goal" (and (Marked a2) (not (Marked a1)))
"end"