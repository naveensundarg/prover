{:name        "cc-soundness-test-1"
 :description "kicking the tires"
 :assumptions {1 A}
 :goal        (or P Q)
 }

{:name        "cc-soundness-test-2"
 :description "Referential opacity should be satisfied"
 :assumptions {1 (not (Knows! a now (= morning_star evening_star)))
               2 (= morning_star evening_star)
               3 (Knows! a now (= morning_star morning_star))}
 :goal        (and P (not P))
 }