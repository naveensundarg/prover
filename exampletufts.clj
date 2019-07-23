{:name        "Example"
 :description "A simple example"


 :assumptions {
                Premise1 (Knows! robot
                                 (not (Knows! brad
                                              (Ought! robot
                                                      (exists [?room ?person]
                                                              (holds (in (room ?room) ?person) now)) NotClean))))
                Premise2 (Knows! robot (holds (in (room 1) brad) now))
                Premise3 (Knows! robot (Ought! robot (exists [?room ?person] (holds (in (room ?room) ?person) now)) NotClean))
                Premise4 (if (Knows! robot
                                     (not (Knows! brad
                                                  (Ought! robot
                                                          (exists [?room ?person]
                                                                  (holds (in (room ?room) ?person) now)) NotClean))))
                           (Says! robot brad (Ought! robot
                                                     (exists [?room ?person]
                                                             (holds (in (room ?room) ?person) now)) NotClean)))

                }
 :goal        (and NotClean (Says! robot brad (Ought! robot
                                                      (exists [?room ?person]
                                                              (holds (in (room ?room) ?person) now)) NotClean)))}
