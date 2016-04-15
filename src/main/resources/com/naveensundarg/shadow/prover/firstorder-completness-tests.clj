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


"begin" "Renaming"
"goal" (if (forall (?x) (P ?x)) (forall (?y) (P ?y)) )
"end"
