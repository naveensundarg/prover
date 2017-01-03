{ :name "axiom selection 1"
 :description ""
 :assumptions {S9 (forall (z)  (= (* z 0) 0))
               S7 (forall (x y) (= (+ x y) (+ y x)))
               S4 (forall (x)  (= (+ x 0) x))
               S5 (forall (z y) (= (* z (+ y 1)) (+ (* z y) z)))
               }

 :goal (forall (z) (= (* z 1) z))
 }
