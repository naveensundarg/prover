{
 DEC1 (forall [?t1 ?f ?t2]
              (iff (StoppedIn ?t1 ?f ?t2)
                   (exists [?e ?t] (and (Happens ?e ?t) (Prior ?t1 ?t) (Prior ?t ?t2) (Terminates ?e ?f ?t)))))

 DEC2 (forall [?t1 ?f ?t2]
              (iff (StartedIn ?t1 ?f ?t2)
                   (exists [?e ?t] (and (Happens ?e ?t) (Prior ?t1 ?t) (Prior ?t ?t2) (Initiates ?e ?f ?t)))))

 ;;; chunk ;;;

 DEC3 (forall [?e ?f1 ?t1 ?f2 ?t2]
              (if (and (Happens ?e ?t1)
                       (Initiates ?e ?f1 ?t1)
                       (Prior 0 ?t2)
                       (Trajectory ?f1 ?t1 ?f2 ?t2)
                       (not (StoppedIn ?t1 ?f1 (+ ?t1 ?t2))))
                (HoldsAt ?f2 (+ ?t1 ?t2))))

 DEC4 (forall [?e ?f1 ?t1 ?f2 ?t2]
              (if (and (Happens ?e ?t1)
                       (Terminates ?e ?f1 ?t1)
                       (Prior 0 ?t2)
                       (AntiTrajectory ?f1 ?t1 ?f2 ?t2)
                       (not (StartedIn ?t1 ?f1 (+ ?t1 ?t2))))
                (HoldsAt ?f2 (+ ?t1 ?t2))))

 ;;;; chunk ;;;
 DEC5 (forall [?f ?t]
              (if (and (HoldsAt ?f ?t) (not (ReleasedAt ?f (+ ?t 1)))
                       (not (exists [?e] (and (Happens ?e ?t) (Terminates ?e ?f ?t)))))
                (HoldsAt ?f (+ ?t 1))))

 DEC6 (forall [?f ?t]
              (if (and (not (HoldsAt ?f ?t)) (not (ReleasedAt ?f (+ ?t 1)))
                       (not (exists [?e] (and (Happens ?e ?t) (Initiates ?e ?f ?t)))))
                (not (HoldsAt ?f (+ ?t 1)))))

 DEC7 (forall [?f ?t]
              (if (and (ReleasedAt ?f ?t)
                       (not (exists [?e] (and (Happens ?e ?t) (or (Initiates ?e ?f ?t) (Terminates ?e ?f ?t))))))
                (ReleasedAt ?f (+ ?t 1))))

 DEC8 (forall [?f ?t]
              (if (and (not (ReleasedAt ?f ?t) )
                       (not (exists [?e] (and (Happens ?e ?t) (Releases ?e ?f ?t)))))
                (not (ReleasedAt ?f (+ ?t 1)))))

  ;;;; chunk ;;;

 DEC9 (forall [?e ?f ?t]
              (if (and (Happens ?e ?t) (Initiates ?e ?f ?t))
                (HoldsAt ?f (+ ?t 1))))

 DEC10 (forall [?e ?f ?t]
              (if (and (Happens ?e ?t) (Terminates ?e ?f ?t))
                (not (HoldsAt ?f (+ ?t 1)))))

 DEC11 (forall [?e ?f ?t]
              (if (and (Happens ?e ?t) (Releases ?e ?f ?t))
                 (ReleasedAt ?f (+ ?t 1))))

 DEC12 (forall [?e ?f ?t]
              (if (and (Happens ?e ?t) (or (Initiates ?e ?f ?t) (Terminates ?e ?f ?t)) )
                 (not (ReleasedAt ?f (+ ?t 1)))))

 }