{
 :name  "TAI AEGAP Example"
 :assumptions
       {
        "Background 1 - Iff the battery is alive it is dead. "
                        (Knows! tai t0
                                (forall [time]
                                        (iff (not (holds battery_alive time))
                                             (holds battery_dead time))))

        "Background 2 - Iff the alarm is on it is off."
                        (Knows! tai t0
                                (forall [time]
                                        (iff (not (holds alarm_on time))
                                             (holds alarm_off time))))

        "Background 3 - If the battery is alive and the CO level is greater than 400,
         the alarm will not be off."
                        (Believes! tai t0
                                   (forall [time]
                                           (if (and (exists [level] (and (> level 400) (holds (COLevel level) time)))
                                                    (holds battery_alive time))
                                             (not (holds alarm_off time)))))
        "Background 4" (Knows! tai t0
                               (forall
                                 [time]
                                 (if
                                   (and (exists [level] (and (> level 400) (holds (COLevel level) time)))
                                        (not (holds battery_alive time)))
                                   (holds dangerous time))))



        "Background 5" (Believes! tai t1
                                  (Ought! tai t1
                                          (holds dangerous t1)
                                          (Intends! tai t1 alert)))

        "Background 6" (Believes! tai t1
                                  (Ought! tai t1
                                          alert
                                          (Desires! tai t1 alert)))

        "Background 7" (Believes! tai t0
                                  (forall [time]
                                          (if (or (not (holds dangerous time))
                                                  alert)
                                            safe)))

        "Backgroung 8" (forall [time] (if (DangerouslyHighCOLevel time)
                                        (not (happens (alert fire-station) time))))

        "Background 9" (forall [time]
                               (iff (HighCOLevel time)
                                    (exists [level]
                                            (and (> level 400)
                                                 (holds (COLevel level) time)))))

        "Background 10" (forall [time]
                                (iff (DangerouslyHighCOLevel time)
                                     (exists [level]
                                             (and (> level 800)
                                                  (holds (COLevel level) time)))))


        "Planning" (if (Desires! tai t1 alert)
                     (Intends! tai t1
                               (or (happens (alert fire-station) t2)
                                   (happens (switch-on speaker) t2))))

        :Situation-1 (Perceives! tai t1 (holds (COLevel 500) t1))
        :Situation-2 (Perceives! tai t1 (holds alarm_off t1))
        :Situation-3 (Perceives! tai t2 (holds (COLevel 1000) t2))

        }



 :goal (happens (switch-on speaker)  t2)
 }