{:name "false-belief-task-M1"
 :description "Method 1 in the False Belief Task Paper"
 :assumptions {1 (Perceives! a1 (happens (action a2 alpha) t))
               2 (Common! (forall (?t) (if (happens (action a2 alpha) ?t)
                                         (Knows! a2  (happens (action a2 alpha) ?t)))))}

 :goal (Knows! a1 (Knows! a2 (happens (action a2 alpha) t)))}


