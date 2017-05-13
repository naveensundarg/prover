


(forall [?x ?p] (if (and (Holds (knight ?x)) (Says ?p)) (Holds ?p)))

(forall [?x ?p] (if (and (Holds (knave ?x))  (Says ?p)) (not (Holds ?p))))