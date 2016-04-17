"begin" "kicking the tires"
"assumption" P
"goal" (or P Q)
"end"

"begin" "socrates is mortal"
"assumption" (forall (?x) (if (Man ?x) (Mortal ?x) ))
"assumption" (Man socrates)
"goal" (Mortal socrates)
"end"

"begin" "Law of Excluded Middle"
"goal" (forall (?x) (or (P ?x) (not (P ?x))))
"end"

"begin" "*fol-true-test-1"
"assumption" (exists (?x) (and  (Human ?x) (Flew-to-Space ?x)))
"assumption" (Flew-to-Space armstrong)
"assumption" (Man armstrong)
"assumption" (forall (?x) (iff (Human ?x) (or (Man ?x) (Woman ?x))))
"goal" (exists (?x) (and (Man ?x) (Flew-to-Space ?x)))
"end"

"begin" "*fol-true-test-1"
"assumption" (forall (?x ?y ?z) (if (and (R ?x ?y) (R ?y ?z)) (R ?x ?z)))
"assumption" (R a b)
"assumption" (R b c)
"goal" (R a c)
"end"

"begin" "*fol-true-test-1"
"assumption" (forall (?x ?y ?z) (if (and (R ?x ?y) (R ?y ?z)) (R ?x ?z)))
"assumption" (forall (?x ?y) (if (R ?x ?y) (R ?y ?x)))
"assumption" (forall (?x) (R ?x))
"assumption" (R a b)
"assumption" (R b c)
"assumption" (R c d)
"goal" (R a d)
"end"

"begin" "*fol-true-test-1"
"assumption" (forall (?x ?y ?z) (if (and (R ?x ?y) (R ?y ?z)) (R ?x ?z)))
"assumption" (forall (?x ?y) (if (R ?x ?y) (R ?y ?x)))
"assumption" (forall (?x) (R ?x))
"assumption" (R a b)
"assumption" (R b c)
"assumption" (R c d)
"goal" (R d a)
"end"

"begin" "Universal implies Existential"
"goal" (if (forall (?x) (P ?x)) (exists (?y) (P ?y)) )
"end"

"begin" "Bird theorem"
"goal" (exists (?x) (if (Bird ?x) (forall (?y) (Bird ?y))))
"end"

"begin" "Everyone likes anyone who likes someone."
"assumption" (likes a b)
"assumption" (forall (?x) (if (exists (?z) (likes ?x ?z)) (forall (?y) (likes ?y ?x))))
"goal" (forall (?x ?y) (likes ?x ?y))
"end"

"begin" "Demodulation Test 1"
"assumption" (= a b)
"assumption" (P a)
"goal" (P b)
"end"

"begin" "Demodulation Test 1"
"assumption" (and (= a b) (= c d))
"assumption" (Q d)
"assumption" (P a)
"assumption" (forall (?x) (if (and (P b) (Q c)) (R ?x)))
"goal" (forall (?x) (R ?x))
"end"

"begin" "Demodulation Test 2: socrates is mortal"
"assumption" (= socrates (husband xanthippe))
"assumption" (forall (?x) (if (Man ?x) (Mortal ?x) ))
"assumption" (Man socrates)
"goal" (Mortal (husband xanthippe))
"end"

"begin" "Demodulation Test 3: socrates is mortal"
"assumption" (= (husband xanthippe) socrates)
"assumption" (forall (?x) (if (Man ?x) (Mortal ?x) ))
"assumption" (Man socrates)
"goal" (Mortal (husband xanthippe))
"end"

"begin" "Demodulation Test 3"
"assumption" (Culprit john)
"goal" (if (= jack john) (Culprit jack))
"end"

"begin" "Suppes Theorem"
"assumption" (forall (?x) (= ?x ?x))
"assumption" (forall (?y) (exists (?x) (or (In ?x ?y) (= ?y EmptySet))))
"assumption" (forall (?z) (exists (?x) (forall (?y) (iff (In ?y ?x) (and (In ?y ?z) (not (= ?y ?y)))))))
"goal" (forall (?x) (not (In ?x EmptySet)))
"end"

