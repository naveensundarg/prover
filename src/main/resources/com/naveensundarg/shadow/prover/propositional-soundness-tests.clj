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

