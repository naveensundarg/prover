"begin" "*prop-nd-true-test-1"
"assumption" P
"goal" P
"end"

"begin" "*prop-nd-true-test-2"
"assumption" P
"assumption" Q
"goal" P
"end"

"begin" "*prop-nd-true-test-3*"
"goal" (if P P)
"end"

"begin" "case prop-nd-true-test-4*"
"goal" (if P (if Q P))
"end"

"begin" "*prop-nd-true-test-5*"
"assumption" P
"assumption" Q
"goal" (and P Q)
"end"

"begin" "*prop-nd-true-test-6*"
"assumption" P
"assumption" Q
"assumption" R
"goal" (and (and R Q) R)
"end"

"begin" "*prop-nd-true-test-7*"
"assumption" P
"assumption" Q
"assumption" R
"goal" (and (and R Q) R)
"end"

"begin" "*prop-nd-true-test-8*"
"assumption" Q
"assumption" R
"goal" (if P (and R (and Q P)))
"end"

"begin" "*prop-nd-true-test-9*"
"assumption" P
"assumption" Q
"goal" P
"end"

"begin" "*prop-nd-true-test-10*"
"assumption" (and P (and Q R))
"assumption" U
"assumption" V
"goal" (and R (and U V))
"end"

"begin" "*prop-nd-true-test-11*"
"assumption" (or P Q)
"goal" (or Q P)
"end"

"begin" "*prop-nd-true-test-12*"
"assumption" (or P (or Q R))
"goal" (or (or R Q) P)
"end"

"begin" "*prop-nd-true-test-13*"
"assumption" (not (or P Q))
"goal" (not P)
"end"

"begin" "*prop-nd-true-test-14*"
"assumption" (not (or P Q))
"goal" (and (not P) (not Q))
"end"

"begin" "*prop-nd-true-test-15*"
"assumption" (and (not P) (not Q))
"goal" (not (or P Q))
"end"

"begin" "*prop-nd-true-test-16*"
"assumption" (not (or (not P) Q))
"goal" P
"end"

"begin" "*prop-nd-true-test-17* kok_o213_8_32"
"assumption" (if H (and E D))
"assumption" (if (or E My) R)
"assumption" (if Ma (not R))
"goal" (if H (not Ma))
"end"

"begin" "*prop-nd-true-test-18* kok_o213_8_35"
"assumption" (if (not Cube_b) Small_b)
"assumption" (if Small_c (or Small_d Small_e))
"assumption" (if Small_d (not Small_c))
"assumption" (if Cube_b (not Small_e))
"goal" (if Small_c Small_b)
"end"

"begin" "*prop-nd-true-test-19* kok_o213_8_35"
"assumption" (and Small_a (or Medium_b Large_c))
"assumption" (if Medium_b Front_Of_a_b)
"assumption" (if Large_c Tet_c)
"goal" (if (not Tet_c) Front_Of_a_b)
"end"

"begin" "*prop-nd-true-test-20*"
"goal" (iff (and A (not B)) (not (if A B)))
"end"

"begin" "*prop-nd-true-test-21*"
"goal" (or P (not P))
"end"

"begin" "*prop-nd-true-test-22*"
"goal" (if P (if Q P))
"end"

"begin" "*prop-nd-true-test-23* 1"
"goal" (if (not (if p q)) (if q p))
"end"

"begin" "*prop-nd-true-test-24* 2"
"goal" (iff (if p q) (if (not q) (not p)))
"end"

"begin" "*prop-nd-true-test-25* 3"
"goal" (if (not (if p q)) (if q p))
"end"

"begin" "*prop-nd-true-test-26* 4"
"goal" (iff  (if (not p) q) (if (not q) p))
"end"

"begin" "*prop-nd-true-test-27* 5"
"goal" (if (if (or p q) (or p r)) (or p (if q r)))
"end"

"begin" "*prop-nd-true-test-28* 7"
"goal" (or P (not (not (not P))))
"end"

"begin" "*prop-nd-true-test-29* 8 (Peirce's Law)"
"goal" (if (if (if p q) p) p)
"end"

"begin" "*prop-nd-true-test-30* Problem 9"
"goal" (if (and (and (or p q) (or (not p) q)) (or p (not q))) (not (or (not p) (not q))))
"end"

"begin" "*prop-nd-true-test-31* Problem 10"
"assumption" (if q r)
"assumption" (if r (and p q))
"assumption" (if p (or q r))
"goal" (if p q)
"end"

"begin" "*prop-nd-true-test-32* Problem 11"
"goal" (if p p)
"end"

"begin" "*prop-nd-true-test-33* Problem 12"
"goal" (iff (iff (iff p q) r) (iff p (iff q r)))
"end"

"begin" "*prop-nd-true-test-34* Problem 13"
"goal" (iff (or p (and q r)) (and (or p q) (or p r)))
"end"

"begin" "*prop-nd-true-test-35*"
"goal" (if (if p (not p)) (if p q))
"end"

"begin" "*prop-nd-true-test-36* testing chaining"
"assumption" p
"assumption" (if p (and w e))
"assumption" (if e (and q  (and r s)))
"goal" r
"end"

"begin" "*prop-nd-true-test-37* testing chaining with a theorem at the start"
"assumption" (if (or p (not p)) (and w e))
"assumption" (if e (and q  (and r s)))
"goal" r
"end"

"begin" "*prop-nd-true-test-38* complex instantiation of a simple theorem"
"goal" (or (if p (if q r)) (not (if p (if q r))))
"end"

"begin" "*prop-nd-true-test-39* testing chaining with a theorem at the start"
"assumption" (if (or (and p q) (not (and p q))) (and w e))
"assumption" (if e (and q  (and r s)))
"goal" r
"end"

"begin" "*prop-nd-true-test-40* Negation of a contradiction"
"goal" (not (and p (not p)))
"end"

"begin" "*prop-nd-true-test-41* Negation of a theorem leads to explosion"
"assumption" (not (or p (not p)))
"goal" z
"end"

"begin" "*prop-nd-true-test-42* Negation of a theorem leads to explosion"
"assumption" (not (or p (not p)))
"goal" (or p q)
"end"

"begin" "*prop-nd-true-test-43*"
"goal" (if (if (if P Q) R) (if P (if Q R)))
"end"

"begin" "*prop-nd-true-test-44*"
"goal" (iff (iff P Q) (iff Q P))
"end"

"begin" "*prop-nd-true-test-45* Biconditional chaining"
"assumption" (iff p (and q r))
"assumption" (iff q (and a b))
"goal" (if p a)
"end"

"begin" "*prop-nd-true-test-46* direction 1 of 33"
"goal" (if (iff (iff p q) r) (iff p (iff q r)))
"end"


