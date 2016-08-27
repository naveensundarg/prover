{:name "mode1-prob1"
 :description "dt16: fol is transparent with Knows!"
 :assumptions {                                             ;;1 (forall (?a ?time ?P) (if (HOLDS (Knows ?a ?time  ?P)) (HOLDS ?P)))
               1 (forall (?a1 ?a2) (iff (and (HOLDS ?a1) (HOLDS ?a2)) (HOLDS (and ?a1 ?a2))))
               2 (HOLDS (and cloudy wet))
 :goal (and (HOLDS cloudy) (HOLDS wet))}