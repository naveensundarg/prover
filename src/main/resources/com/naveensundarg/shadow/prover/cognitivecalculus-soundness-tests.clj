"begin" "kicking the tires"
agent
"assumption" A
"goal" (or P Q)
"end"

"begin" "Referential opacity should be satisfied"
"assumption" (not (Knows! a now (= morning_star evening_star)))
"assumption" (= morning_star evening_star)
"assumption" (Knows! a now (= morning_star morning_star))
"goal" (and P (not P))
"end"