{:name        "dt16"
 :description "dt16: fol is transparent with Knows!"
 :assumptions {1 (not (Knows a now (= morning_star evening_star)))
               2 (= morning_star evening_star)
               3 (Knows a now (= morning_star morning_star))}
 :goal        (and P (not P))}
