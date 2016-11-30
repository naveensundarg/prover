{:name        "*cognitive-calculus-closure-test-5*"
 :description "Testing the ought rule"
 :assumptions {1 (Common! t0 (if (and P Q) R))
               ;; It is commonly known that if someone is honest and cries for help, then they are injured.

               2 (Common! t0 (Ought! robot2 t2 R A))

               3 (Believes! robot1 t2 (Believes! robot2 t0 P))

               4 (Common! t1 Q)

               }
 :goal         (Believes! robot1 t2 A)}

