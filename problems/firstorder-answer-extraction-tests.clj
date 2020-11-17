;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Test 1"
 :description "A simple test"
 :assumptions {A1 (role bill (supervisor self))
               A2 (forall (A B) (implies (role A (supervisor B)) (is_superior A B)))
               A3 (forall X (iff (unsafe X) (permitted self (not X))))
               A4 (forall (A B X) (implies (and (want B X) (is_superior B A))
                                           (obligated A X)))
               A5 (forall (A X) (implies (and (obligated A X) (not (permitted A (not X))))
                                         (goal A X)))
               A6 (want bill (did (walk self straight)))

               A7 (not (unsafe (did (walk self straight))))
               }

 :answer-variables [?x]

 :answers-expected ([(did (walk self straight))])

 :goal        (goal self ?x)
 }


{:name        "Test 2"
 :description "A simple test"
 :assumptions {A1 (role bill (supervisor self))
               A2 (forall (A B) (implies (role A (supervisor B)) (is_superior A B)))
               A3 (forall X (iff (unsafe X) (permitted self (not X))))
               A4 (forall (A B X) (implies (and (want B X) (is_superior B A))
                                           (obligated A X)))
               A5 (forall (A X) (implies (and (obligated A X) (not (permitted A (not X))))
                                         (goal A X)))
               A6 (want bill (did (walk self straight)))

               A7 (not (unsafe (did (walk self straight))))
               }

 :answer-variables [?x ?y]

 :answers-expected ([self straight])

 :goal        (goal self (did (walk ?x ?y)))
 }

{:name        "Test 3"
 :description "A simple test"
 :assumptions {A1 (not (= a b))
               }

 :answer-variables [?x ?y]

 :answers-expected ([a b]
                     [b a])

 :goal        (not (= ?x ?y))
 }


{:name        "Test 4"
 :description "A bit more tougher"
 :assumptions {A1 (forall (?x ?y ?z) (implies (and (R ?x ?y) (R ?y ?z))
									   (R ?x ?z)))
               A2 (forall (?x ?y) (implies (R ?x ?y) (R ?y ?x)))

               A3 (and (R a b)
                       (R b c)
                       (R c d))}

 :answer-variables [?x ?y]

 :answers-expected ([c d]
                     [d c]
                     [b c]
                     [c b]
                     [a b]
                     [b a]
                     [c c]
                     [d b]
                     [b d]
                     [d d]
                     [b b]
                     [c a]
                     [a c]
                     [a a]
                     [d a]
                     [a d])

 :goal        (R ?x ?y)}


{:name        "Test 1"
 :description "A simple test"
 :assumptions {
               1 (forall [x y room] (if (and (not (= x y)) (and (in x room) (in y room))) (sameroom x y)))
               2 (in (agent 1) room1)
               3 (in (agent 2) room1)
               4 (in (agent 3) room1)
               5 (forall [x y] (if (not (= x y)) (not (= (agent x) (agent y)))))
               }

 :answer-variables [?x ?y]

 :answers-expected ([(agent 1) (agent 2)]
                     [(agent 2) (agent 1)]
                     [(agent 3) (agent 1)]
                     [(agent 1) (agent 3)]
                     [(agent 2) (agent 3)]
                     [(agent 3) (agent 2)]
                     )

 :goal        (sameroom ?x ?y)
 }



{:name        "Test Belief answer"
 :description "A simple test"
 :assumptions {1 (Believes!  a P)}

 :answer-variables [?a]

 :answers-expected ([a])

 :goal        (Believes! ?a P)
 }
