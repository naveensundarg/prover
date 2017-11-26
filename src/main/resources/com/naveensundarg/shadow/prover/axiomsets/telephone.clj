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

 ;;; A call may go unanswered ;;

 ;; If p1 is ringing p2 and an agent sets down p1, p1 will be idle
 A3.12 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Ringing ?p1 ?p2) ?t)
                 (Initiates (SetDown ?a ?p1) (Idle ?p1) ?t)))

  ;; If p1 is ringing p2 and an agent sets down p1, p2 will be idle
 A3.13 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Ringing ?p1 ?p2) ?t)
                 (Initiates (SetDown ?a ?p1) (Idle ?p2) ?t)))

 A3.14 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Ringing ?p1 ?p2) ?t)
                 (Terminates (SetDown ?a ?p1) (Ringing ?p1 ?p2) ?t)))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 6 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; A call may go unanswered ;;

 ;; If p1 is ringing p2 and an agent picks up p2, both the phones will be connected.
 A3.15 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Ringing ?p1 ?p2) ?t)
                 (Initiates (PickUp ?a ?p2) (Connected ?p1 ?p2) ?t)))

 ;; If p1 is ringing p2 and an agent picks up p2, p1 will no longer be ringing p2.
 A3.16 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Ringing ?p1 ?p2) ?t)
                 (Terminates (PickUp ?a ?p2) (Ringing ?p1 ?p2) ?t)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 7 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; A call may be completed

 ;; If p1 is connected to p2 and an agent sets down p1; then p1 will be idle.
 A3.17 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Connected ?p1 ?p2) ?t)
                 (Initiates (SetDown ?a ?p1) (Idle ?p1) ?t)))

 ;; If p1 is connected to p2 and an agent sets down p1; then p2 will be disconnected.
 A3.18 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Connected ?p1 ?p2) ?t)
                 (Initiates (SetDown ?a ?p1) (Disconnected ?p2) ?t)))

 ;; If p1 is connected to p2 and an agent sets down p1; then p1 will no longer be connected to p2.
 A3.19 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Connected ?p1 ?p2) ?t)
                 (Terminates (SetDown ?a ?p1) (Connected ?p1 ?p2) ?t)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 8 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; If p1 is connected to p2 and an agent sets down p2; then p2 will be idle.

  A3.20 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Connected ?p1 ?p2) ?t)
                 (Initiates (SetDown ?a ?p2) (Idle ?p2) ?t)))

 ;; If p1 is connected to p2 and an agent sets down p1; then p1 will be disconnected.
 A3.21 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Connected ?p1 ?p2) ?t)
                 (Initiates (SetDown ?a ?p2) (Disconnected ?p1) ?t)))

 ;; If p1 is connected to p2 and an agent sets down p1; then p1 will no longer be connected to p2.
 A3.22 (forall [?a ?p1 ?p2 ?t]
               (if (HoldsAt (Connected ?p1 ?p2) ?t)
                 (Terminates (SetDown ?a ?p2) (Connected ?p1 ?p2) ?t)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 9 ;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; If an agent sets down a phone that is disconnected, the phone will be idle
 A3.23 (forall [?a ?p ?t]
               (if (HoldsAt (Disconnected ?p) ?t)
                 (Initiates (SetDown ?a ?p) (Idle ?p) ?t)))

 A3.24 (forall [?a ?p ?t]
               (if (HoldsAt (Disconnected ?p) ?t)
                 (Terminates (SetDown ?a ?p) (Disconnected ?p) ?t)))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 10 ;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; Specifics

 A3.25 (forall [?p] (HoldsAt (Idle ?p) 0))

 A3.26 (forall [?p] (not (HoldsAt (DialTone ?p) 0)) )

 A3.27 (forall [?p] (not (HoldsAt (BusySignal ?p) 0)) )

 A3.28 (forall [?p1 ?p2] (not (HoldsAt (Ringing ?p1 ?p2) 0)) )

 A3.29 (forall [?p1 ?p2] (not (HoldsAt (Connected ?p1 ?p2) 0)) )

 A3.30 (forall [?p] (not (HoldsAt (Disconnected ?p) 0)) )

 A3.31 (forall [?f ?t] (not (ReleasedAt ?f ?t)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;; Chunk 11 ;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; Delta

 ADelta (forall [?e ?t]
                (iff (Happens ?e  ?t)
                     (or (and (= ?e (PickUp agent1 phone1)) (= ?t 0))
                         (and (= ?e (Dial agent1 phone1 phone2)) (= ?t 1))
                         (and (= ?e (PickUp agent2 phone2)) (= ?t 2)))))


;; Lemma1 (not (exists [?e] (and (Happens ?e 0) (Terminates ?e (Idle phone1) 0)) ))

 Lemma (not (exists [?e] (and (Happens ?e 0) (Terminates ?e (Idle phone2) 0)) ))


 }