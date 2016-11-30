{:name        "*cognitive-calculus-closure-test-5*"
 :description "Testing the ought rule"
 :assumptions {
               1 (Common! t0 (Ought! robot2 t2 (injured mary) (happens (action robot2 (help mary)) t2)))


               }
 :goal        (Believes! robot1 t2 (Ought! robot2 t2 (injured mary) (happens (action robot2 (help mary)) t2))) }