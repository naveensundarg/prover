{:name "false-belief-task-M1"
 :description "Method 1 in the False Belief Task Paper"
 :assumptions {1 (Perceives! a1 (happens (action a2 alpha) t))
               2 (Common! (forall (?a ?alpha ?t) (if (happens (action ?a ?alpha) ?t)
                                         (Knows! ?a  (happens (action ?a ?alpha) ?t)))))}

 :goal (Knows! a1 (Knows! a2 (happens (action a2 alpha) t)))}


