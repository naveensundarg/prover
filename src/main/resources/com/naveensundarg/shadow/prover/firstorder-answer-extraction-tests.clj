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

 :answerVariable ?x

 :expectedAnswer (did (walk self straight))

 :goal        (goal self ?x)
 }
