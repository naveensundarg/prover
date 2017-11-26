{
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 1 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; If a phone is idle, picking it put gives us the dial tone.
 A3.1  (forall [?a ?p ?t]
               (if (HoldsAt (Idle ?p) ?t)
                 (Initiates (PickUp ?a ?p) (DialTone ?p) ?t)))

 ; If a phone is idle, picking it put terminates the idle state.
 A3.2  (forall [?a ?p ?t]
               (if (HoldsAt (Idle ?p) ?t)
                 (Terminates (PickUp ?a ?p) (Idle ?p) ?t)))

 ;; If you set down a phone with a dial tone, it will become idle.
 A3.3  (forall [?a ?p ?t]
               (if (HoldsAt (DialTone ?p) ?t)
                 (Initiates (SetDown ?a ?p) (Idle ?p) ?t)))

 ;; If you set down a phone with a dial tone, it will terminate the dial tone.
 A3.4  (forall [?a ?p ?t]
               (if (HoldsAt (DialTone ?p) ?t)
                 (Terminates (SetDown ?a ?p) (DialTone ?p) ?t)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 2 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; If phone 2 is idle and phone 1 has dial tone, dialing phone 2 from phone 1 will initiate ringing of phone 2 from phone 1
 A3.5  (forall [?p1 ?p2 ?a ?t]
               (if (and (HoldsAt (DialTone ?p1) ?t)
                        (HoldsAt (Idle ?p2) ?t))
                 (Initiates (Dial ?a ?p1 ?p2) (Ringing ?p1 ?p2) ?t)))

 ;; If phone 2 is idle and phone 1 has dial tone, dialing phone 2 from phone 1 will terminate the dial tone at phone 1
 A3.6  (forall [?p1 ?p2 ?a ?t]
               (if (and (HoldsAt (DialTone ?p1) ?t)
                        (HoldsAt (Idle ?p2) ?t))
                 (Terminates (Dial ?a ?p1 ?p2) (DialTone ?p1) ?t)))

 ;; If phone 2 is idle and phone 1 has dial tone, dialing phone 2 from phone 1 will terminate the idle state at phone 2
 A3.7  (forall [?p1 ?p2 ?a ?t]
               (if (and (HoldsAt (DialTone ?p1) ?t)
                        (HoldsAt (Idle ?p2) ?t))
                 (Terminates (Dial ?a ?p1 ?p2) (Idle ?p2) ?t)))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 3 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; If phone 2 is NOT idle and phone 1 has dial tone, dialing phone 2 from phone 1 will give us a busy signal at phone 1
 A3.8  (forall [?p1 ?p2 ?a ?t]
               (if (and (HoldsAt (DialTone ?p1) ?t)
                        (not (HoldsAt (Idle ?p2) ?t)))
                 (Initiates (Dial ?a ?p1 ?p2) (BusySignal ?p1) ?t)))

 ;; If phone 2 is NOT idle and phone 1 has dial tone, dialing phone 2 from phone 1 will terminate the dial tone at phone 1
 A3.9  (forall [?p1 ?p2 ?a ?t]
               (if (and (HoldsAt (DialTone ?p1) ?t)
                        (not (HoldsAt (Idle ?p2) ?t)))
                 (Terminates (Dial ?a ?p1 ?p2) (DialTone ?p1) ?t)))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 4 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; If an agent sets down a phone with a busy signal, it will no longer be idle.
 A3.10 (forall [?a ?p ?t]
               (if (HoldsAt (BusySignal ?p) ?t)
                 (Initiates (SetDown ?a ?p) (Idle ?p) ?t)))

 ;; If an agent sets down a phone with a busy signal, the busy signal will be terminated.
 A3.11 (forall [?a ?p ?t]
               (if (HoldsAt (BusySignal ?p) ?t)
                 (Terminates (SetDown ?a ?p) (BusySignal ?p) ?t)))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 5 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; If p1 is ringing p2 and p1 is set down, then p1 will be idle
 A3.12 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Ringing ?p1 ?p2) ?t)
                 ))

 }