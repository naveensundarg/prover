"begin" "*fol-true-test-1"
"assumption" (forall (?x ?y ?z) (if (and (R ?x ?y) (R ?y ?z)) (R ?x ?z)))
"assumption" (R a b)
"assumption" (R b c)
"assumption" (R c d)
"goal" (R a d)
"end"