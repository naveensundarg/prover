v

  {:name "Example 2"
   :assumptions {:f1 q
                 :f2 (or (not q) (Believes! a1 r))
                 :f3 (Believes! a1 (or (not s) (not r)))
                 :f4 (Believes! a2 (not q))
                 :f5 (Believes! a1 (Believes! a2 r))
                 :f6 (Believes! a1 (Believes! a2 s))}

   :goal (Believes! a1 (not s))}




;
;  {:name "Example 2"
;   :assumptions {
;
;                  :f1 (not (Believes! a P))
;                  :f2 (Believes! a (if (exists [?x] (Q ?x)) P)
;                  :f3 (Believes! a (forall [?x] (Q ?x)))
;
;   :goal (Believes! a1 (not s))}
;
;
;
;
;  {:name        "Self Sacrifice"
; :description ""
; :assumptions {A1 (Knows! I now (forall [?x] (if (Agent ?x) (or (= ?x I) (= ?x P1) (= ?x P2) (= ?x P3)))))
;               A2 (Knows! I now (= 1 (nu alpha I P1 now)))
;               A3 (Knows! I now (= 1 (nu alpha I P2 now)))
;               A4 (Knows! I now (= 1 (nu alpha I P3 now)))
;               A5 (Knows! I now (= (- 1) (nu alpha I I now)))
;               A6 (Knows! I now (= alpha (Drop (self I) track1 3)))
;               A7 (Common! (forall [a b] (if (= (self a) (self b)) (= a b))))
;               A8 (Knows! I now (forall [a] (= a (self a))))
;               A9 (Knows! I now (and (not (= P1 P2)) (not (= P1 P3)) (not (= P1 I)) (not (= P2 P3)) (not (= P2 I)) (not (= P3 I))))
;               A10 (Knows! I now (and (>> 1 0) (<< -1 0)))}
;
; :goal       (Knows! I now (forall [?agent] (if (Agent ?agent) (if (not (= ?agent (self I))) (>> (nu alpha I ?agent now) 0))))) }
;




  (exists (a) (exists (b) (exists (c) (and (isShape a cube)
                                           (isShape b cube)
                                           (isShape c cube)
                                           (not (= a b))
                                           (not (= a c))
                                           (not (= b c))
                                           (not (exists (q) (and (isShape q cube)
                                                                 (not (= q a))
                                                                 (not (= q b))
                                                                 (not (= q c)) )))))))


  (implies
    (exists (a)
            (exists (b)
                    (exists (c)
                            (and (isShape a cube) (isShape b cube) (isShape c cube) (not (= a b)) (not (= a c)) (not (= b c))))))
    (exists (a) (exists (b c) (and (isShape a cube) (isShape b cube) (not (= a b))))))
