
{:name        "*cognitive-calculus-ouginjuredht-test-4*"
 :description "Testing the ought rule"
 :assumptions {1 (Believes! robot now INJURED)
               2 (Common! now (Ought! robot now INJURED HELP))
               }
 :goal        (happens (action robot HELP) now)}