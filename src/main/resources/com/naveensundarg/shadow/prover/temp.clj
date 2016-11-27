
{:name        "*cognitive-calculus-ouginjuredht-cl-4*"
 :description "Testing the ought rule"
 :assumptions {1 (Believes! robot t1 (forall (?x) (if (Man ?x) (Mortal ?x))))
               2 (Believes! robot t2 (Man socrates))
               }
 :goal        (Believes! robot t3 (Mortal socrates))}