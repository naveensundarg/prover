;;; these should not be proven!

"begin" "*prop-nd-false-test-1*"
"goal" P
"end"

"begin" "*prop-nd-false-test-2*"
"assumption" (or p q)
"goal" P
"end"

"begin" "*prop-nd-false-test-3*"
"assumption" (and p q)
"goal" r
"end"

"begin" "*prop-nd-false-test-4*"
"assumption" (if p q)
"goal" p
"end"

"begin" "*prop-nd-false-test-5*"
"assumption" (if p q)
"goal" q
"end"

"begin" "*prop-nd-false-test-6*"
"assumption" (if p q)
"goal" (not (or p (not q)))
"end"

"begin" "*prop-nd-false-test-7*"
"assumption" (if p q)
"assumption" (not (not (or p (not q))))
"goal" (not (or p (not q)))
"end"

"begin" "*prop-nd-false-test-8*"
"goal" (and p (not p))
"end"

"begin" "*prop-nd-false-test-9*"
"goal" (not (not (and p (not P))))
"end"

"begin" "*prop-nd-false-test-10*"
"goal" (if p (not p))
"end"

"begin" "Universal implies Existential"
"goal" (if (exists (?x) (P ?x)) (forall (?y) (P ?y)) )
"end"

"begin" "Suppes Theorem"
"assumption" (forall (?x) (= ?x ?x))
"assumption" (forall (?y) (exists (?x) (or (In ?x ?y) (= ?y EmptySet))))
"assumption" (forall (?z) (exists (?x) (forall (?y) (iff (In ?y ?x) (and (In ?y ?z) (not (= ?y ?y)))))))
"goal" (forall (?x) (not (In ?x U)))
"end"

"begin" "occurs"
"goal" (if (forall (?x) (exists (?y) (Loves ?x ?y))) (exists (?y) (forall (?x) (Loves ?x ?y))))
"end"