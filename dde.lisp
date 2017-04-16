
(in-package :snark-user)



(defun declare-ec-sort-system ()
  (declare-sort 'Fluent)
  (declare-sort 'Event)
  (declare-subsort 'Action 'Event)
  (declare-sort 'ActionType)
  
  (declare-relation 'Initiates 3 :sort '(Event Fluent Number))
  (declare-relation 'Terminates 3 :sort '(Event Fluent Number))
  (declare-relation 'Releases 3 :sort '(Event Fluent Number))

  (declare-relation 'InitiallyP 1 :sort '(Fluent))
  (declare-relation 'InitiallyN 1 :sort '(Fluent))
 
  (declare-relation 'Happens 2 :sort '(Event Number))
  (declare-relation 'Happens 3 :sort '(Event Number Number))
  (declare-relation 'HoldsAt 2 :sort '(Fluent Number))
  
  (declare-relation 'Clipped 3 :sort '(Number Fluent Number))
  (declare-relation 'DeClipped 3 :sort '(Number Fluent Number))

  (declare-relation 'Trajectory 4 :sort '(Fluent Number Fluent Number))
  
  (declare-relation 'Prior 2 :sort '(Number Number))
  
    
  (declare-constant 't0 :sort 'Number)
  (declare-constant 't1 :sort 'Number)
  (declare-constant 't2 :sort 'Number)
  (declare-constant 't3 :sort 'Number)
  (declare-constant 't4 :sort 'Number)
  (declare-constant 'unit :sort 'Number))




(defparameter *SC1* '(forall ((?f Fluent) (?t Number))
		      (implies (and (InitiallyP ?f) (not (Clipped t0 ?f ?t)))
		       (HoldsAt ?f ?t))))

(defparameter *SC2* '(forall ((?t1 Number) (?t2 Number) (?e Event) (?f Fluent))
			    (implies (and (Happens ?e ?t1)
					  (Initiates ?e ?f ?t1)
					  (Prior ?t1 ?t2)
					  (not (Clipped ?t1 ?f ?t2)))
				     (HoldsAt ?f ?t2))))


(defparameter *SC3* '(forall ((?t1 Number) (?f Fluent) (?t2 Number))
		      (iff (Clipped ?t1 ?f ?t2 )
		       (exists ((?e Event) (?t Number))
			(and (Happens ?e ?t)
			     (Prior ?t1 ?t)
			     (Prior ?t ?t2)
			     (Terminates ?e ?f ?t))))))



(defparameter *XC9*
  '(forall ((?e Event) (?f Fluent) (?g Fluent) (?t1 Number) (?t2 Number) (?delta Number))
    (implies (and
	      (Happens ?e ?t1)
	      (Initiates ?e ?f ?t1)
	      (not (Clipped ?t1 ?f ?t2))
	      (Trajectory ?f ?t1 ?g ?delta)
	      (= ?t2 (+ ?t1 ?delta)))
     (HoldsAt ?g ?t2))))


(defparameter *EC-Axioms* (list *SC1* *SC2* *SC3* *XC9*))



(defun declare-dde-commons ()
  (declare-ec-sort-system)
  (snark:declare-sort 'Track)
  (snark:declare-sort 'Moveable :subsorts-incompatible t)
  (snark:declare-subsort 'Agent 'Moveable)
  (snark:declare-subsort 'Trolley  'Moveable)

  (declare-function 'action 2 :sort '(Action Agent ActionType))

  
  (declare-constant 'I :sort 'Agent)
  (snark:declare-constant 'P1 :sort 'Agent)
  (snark:declare-constant 'P2 :sort 'Agent)
  (snark:declare-constant 'P3 :sort 'Agent)
  
  (snark:declare-constant 'trolley :sort 'Trolley)
  (snark:declare-constant 'track1 :sort 'Track)
  (snark:declare-constant 'track2 :sort 'Track)
  (snark:declare-function '+ 2 :sort '(Number Number Number))

  (snark:declare-function 'position 3 :sort '(Fluent Moveable Track Number))

  (snark:declare-function 'switch 2 :sort '(ActionType Track Track))
  (snark:declare-relation 'SwitchPoint 2 :sort '(Track Number))
  (snark:declare-relation 'Heavy 1 :sort '(Agent))
  (snark:declare-function 'dead 1 :sort '(Fluent Agent))
  (snark:declare-function 'onrails 2 :sort '(Fluent Trolley Track))
  (snark:declare-function 'damage 1 :sort '(Event Trolley))
  (snark:declare-constant 'start :sort 'Event)
  
  
  (snark:declare-constant 'motion :sort 'Fluent)
  
  (snark:declare-function 'drop 3 :sort '(ActionType Agent Track Number)))


(defun +-REWRITER (term subst)
    (let ((c1 (first (args term)))
	  (c2 (second (args term))))
      (dereference c1 subst
                   :if-constant
                   (dereference c2 subst
				:if-constant
				(+ c1 c2)
				:if-variable none
				:if-compound none)
                   :if-variable none
                   :if-compound none)))


(defun assert-add-table (end)
  (loop for i from 0 to end do
       (loop for j from 0 to end do
	    (if (< i j)
		(assert `(Prior ,i ,j)))
	    (if (< j i)
		(assert `(Prior ,j ,i)))
	    (assert `(= ,(+ i j) (+ ,i ,j)))
	    (assert `(= ,(+ i j) (+ ,j ,i ))))))

(defun assert-domain (end)
  (assert `(forall ((?p Number))
	    (implies (Prior ?p ,end)
	     ,(cons 'or (loop for i from 0 to end collect 
		       `(= ,i ?p)))))))


(defparameter *horizon* 6)
(defparameter *arithmetic-max* 10)

(defun assert-common-dde-axioms! ()
  (mapcar #'assert *EC-Axioms*)
  
  (assert-add-table *arithmetic-max*)
  (assert-domain *horizon*)
  

  
  ;; Basic Trajectory Axiom.
  (assert '(forall ((?trolley Trolley) (?track Track) (?s Number) (?t Number))
	    (Trajectory	     
	     (onrails ?trolley ?track)
	     ?s
	     (position ?trolley ?track ?t)
	     ?t)))
  
  ;; On a single track, any moveable can be in just one position.
  ;; D2
  (assert '(forall ((?p1 Number) (?p2 Number) (?m Moveable) (?t Track) (?time Number))
	    (implies (and (HoldsAt (position ?m ?t ?p1) ?time) (HoldsAt (position ?m ?t ?p2) ?time))
	     (= ?p1 ?p2)))
	   :documentation "moveable in one position only")
  
  ;; Starting the simulation places the trolley on track1.
  ;; D3
  (assert '(forall ((?t Number)) (Initiates start (onrails trolley track1) ?t))
	  :documentation "we start on track 1") 
  
  ;; We start the simulation at time 0. 
  ;; D4
  (assert '(Happens start 0)
	  
	  )
  
  
  ;; Switching from track 1 to track 2, initiates onrails for track 2.
  ;; D5
  (assert '(forall ((?a Agent) (?t Number) (?track1 Track) (?track2 Track))
	    (Initiates (action ?a (switch ?track1 ?track2)) (onrails trolley ?track2) ?t)))
  
  ;; Switching from track 1 to track 2, terminates onrails for track 1.
  ;; D6
  (assert '(forall ((?a Agent) (?t Number) (?track1 Track) (?track2 Track))
	    (Terminates (action ?a (switch ?track1 ?track2)) (onrails trolley ?track1) ?t)))
  
  ;; if for any track, trolley, person, time and position, the trolley and person are at the same position
  ;; then the person is dead at the next time step. 
  ;; D7
  (assert '(forall ((?track Track) (?trolley Trolley) (?person Agent) (?time Number) (?pos Number))
	    (implies (and 
		  (HoldsAt (position ?trolley ?track ?pos) ?time)
		  (HoldsAt (position ?person ?track ?pos) ?time))
	     (HoldsAt (dead ?person) (+ 1 ?time)))))
  
  ;;initial conditions
  ;; At time 0, no trolleys are present on any track. 
  ;; D8
  (assert '(forall ((?p Number) (?trolley Trolley) (?track Track)) 
	    (not (HoldsAt (position ?trolley ?track ?p) 0))))
  

  ;;; Universal of the three conditions below
  ;; D9
  (assert `(forall ((?track Track) (?person Agent))
		  (implies 
		   (exists ((?position Number))
			   (and (forall ((?t Number)) (HoldsAt (position ?person ?track ?position) ?t))
				(forall ((?t Number))
					(implies (Prior ?t ,*horizon*) 
						 (not (HoldsAt (position trolley ?track ?position) ?t)))))) 
	       (forall ((?t Number))
		       (implies (Prior ?t ,*horizon*) 
				(not (HoldsAt (dead P1) ?t)))))))
 ;; Condition for P1 TODO: Universalize
  ;; D10
  (assert `(implies 
  	    (forall ((?t Number))
  		    (implies (Prior ?t ,*horizon*) (not (HoldsAt (position trolley track1 4) ?t))))
  	    (forall ((?t Number))
  		    (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P1) ?t))))))

   ;; Condition for P2 TODO: Universalize
  ;; D11
  (assert `(implies 
  	    (forall ((?t Number))
  		    (implies (Prior ?t ,*horizon*) (not (HoldsAt (position trolley track1 5) ?t))))
  	    (forall ((?t Number))
  		    (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P2) ?t))))))

  ;; Condition for P3 TODO: Universalize
  ;; D12
  (assert `(implies 
  	    (forall ((?t Number))
  		    (implies (Prior ?t ,*horizon*) (not (HoldsAt (position trolley track2 3) ?t))))
  	    (forall ((?t Number))
  		    (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P3) ?t))))))

  ;; If nothing hits a person, they are not dead.
  ;; D13
  (assert '(forall ((?trolley Trolley) (?person Agent) (?track Track) (?pos Number))
  	    (implies
  	     (not 
  	      (exists ((?t Number)) 
  		      (and (HoldsAt (position ?trolley ?track ?pos) ?t)
  			   (HoldsAt (position ?person ?track ?pos) ?t))))
  	     (not (exists ((?t Number)) (Holds (dead ?person) ?t))))))
 
  ;;D14
  (assert '(forall ((?t Number)) (HoldsAt (position P1 track1 4) ?t)))
  
  ;;D15
  (assert '(forall ((?t Number)) (HoldsAt (position P2 track1 5) ?t)))
  
  
  
  ;; The tracks are different.
  ;;D16
  (assert '(not (= track1 track2)))
  
  ;; In a given track, the trolley can be at only one position.
  ;; (assert '(forall ((?pos1 Number) (?pos2 Number) (?t Number) (?trolley Trolley) (?track Track))
  ;; 	    (implies (and 
  ;; 		      (not (= ?pos1 ?pos2))
  ;; 		      (HoldsAt (position ?trolley ?track ?pos1) ?t))
  ;; 	     (not (HoldsAt (position ?trolley ?track ?pos2) ?t)))))
  
  ;;; Dropping a person onto a track 
  ;; D17
  (assert '(forall ((?a1 Agent) (?a2 Agent) (?track Track) (?position Number) (?t Number))
	    (Initiates (action ?a1 (drop ?a2 ?track ?position))
	     (position ?a2 ?track ?position) ?t)))
  
  
  ;;; Agents are stuck to tracks. 
  ;; D18
  (assert  '(forall ( (?track Track) (?position Number) (?t1 Number) (?t2 Number) (?a Agent)) 
	     (not (Clipped ?t1 (position ?a ?track ?position) ?t2))))
  
  ;; damaged 
  ;; D19
  (assert '(forall ((?d Number) (?trolley Trolley) (?track Track) (?position Number))
  	    (implies (and (HoldsAt (position ?trolley ?track ?position) ?d) (Happens (damage ?trolley) ?d))
  	    
	     (and (HoldsAt (position ?trolley ?track ?position) (+ 1 ?d))
	      (forall ((?t Number)) (implies (Prior (+ 1 ?d) ?t) (HoldsAt (position ?trolley ?track ?position) ?t)))))))
  
  
  
  
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Base Setup ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup ()
  (setup-snark :verbose nil)
  (declare-dde-commons)
  (assert-common-dde-axioms!))

(defun cprint (x) (print x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Helper Run Code ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-scenario (setup name)
  (funcall setup)
  (print name)
  (if (equalp :PROOF-FOUND (prove `(forall ((?t Number)) (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P1) ?t))))))
      (cprint "P1 Alive")
      (cprint "P1 Dead")) 
  
  (funcall setup)
  (if (equalp :PROOF-FOUND (prove `(forall ((?t Number)) (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P2) ?t))))))
      (cprint "P2 Alive")
      (cprint "P2 Dead"))
 
  (funcall setup)
  (if (equalp :PROOF-FOUND (prove `(exists ((?t Number))  (HoldsAt (dead P3) ?t))))
      (cprint "P3 Dead")
      (cprint "P3 Alive"))
  nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Scenario 1 Base ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;28 + 36

(defun scenario-1-base ()

  (setup)
  
  (assert '(forall ((?trolley Trolley) (?track1 Track) (?track2 Track)
		    (?pos Number) (?time Number))
	    (implies (and (not (= ?track1 ?track2))  )
	     (implies (HoldsAt (position ?trolley ?track1 ?pos) ?time)
	      (or
	       (or (= ?pos 0) (= ?pos 2))
	       (not (exists ((?p Number)) (HoldsAt (position ?trolley ?track2 ?p) ?time) )))))))
  
  (assert '(forall ((?t Number)) (HoldsAt (position P3 track2 3) ?t)))

  ;;; These common from perception. 
  (assert '(not (Clipped 0 (onrails trolley track1) 1)))
  (assert '(not (Clipped 0 (onrails trolley track1) 2)))
  (assert '(not (Clipped 0 (onrails trolley track1) 3)))
  (assert '(not (Clipped 0 (onrails trolley track1) 4)))
  (assert '(not (Clipped 0 (onrails trolley track1) 5)))
  (assert '(not (Clipped 0 (onrails trolley track1) 6))))

(defun run-scenario-1-base ()

  (print "Base")
  (scenario-1-base)
  (if (equalp :PROOF-FOUND (prove `(exists ((?t Number)) (HoldsAt (dead P1) ?t))))
      (cprint "P1 Dead")
      (cprint "P1 Alive"))
  
   (scenario-1-base)
   (if (equalp :PROOF-FOUND (prove `(exists ((?t Number)) (HoldsAt (dead P2) ?t))))
      (cprint "P2 Dead")
      (cprint "P2 Alive"))
   
   (scenario-1-base)
   (if (equalp :PROOF-FOUND (prove `(forall ((?t Number)) (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P3) ?t))))))
       (cprint "P3 Alive")
       (cprint "P3 Dead"))
   
   nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Scenario 1 Action ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scenario-1-action ()

  (setup)
  (assert '(forall ((?trolley Trolley) (?track1 Track) (?track2 Track)
		    (?pos Number) (?time Number))
	    (implies (and (not (= ?track1 ?track2))  )
	     (implies (HoldsAt (position ?trolley ?track1 ?pos) ?time)
	      (or
	       (or (= ?pos 0) (= ?pos 2))
	       (not (exists ((?p Number)) (HoldsAt (position ?trolley ?track2 ?p) ?time) )))))))
  (assert '(forall ((?t Number)) (HoldsAt (position P3 track2 3) ?t)))


  ;; these come from perception
  (assert '(not (Clipped 0 (onrails trolley track1) 1)))
  (assert '(not (Clipped 0 (onrails trolley track1) 2)))
  (assert '(not (Clipped 1 (onrails trolley track1) 1)))
  
  (assert '(Clipped 2 (onrails trolley track1) 3 ))
  (assert '(Clipped 2 (onrails trolley track1) 4 ))
  (assert '(Clipped 2 (onrails trolley track1) 5 ))
  (assert '(not (exists ((?a Agent) (?t Number))
		 (Happens (action ?a (switch track2 track1)) ?t))))
  (assert '(forall ((?t2 Number))
	    (not  (Clipped 2 (onrails trolley track2) ?t2))))
  (assert '(Happens (action I (switch track1 track2)) 2)))


(defun run-scenario-1-action ()
  (run-scenario #'scenario-1-action "Scenario 1: Throw the Switch"))


(defun scenario-1-means ()

  (setup)
  (assert '(forall ((?trolley Trolley) (?track1 Track) (?track2 Track)
		    (?pos Number) (?time Number))
	    (implies (and (not (= ?track1 ?track2))  )
	     (implies (HoldsAt (position ?trolley ?track1 ?pos) ?time)
	      (or
	       (or (= ?pos 0) (= ?pos 2))
	       (not (exists ((?p Number)) (HoldsAt (position ?trolley ?track2 ?p) ?time) )))))))

  ;(assert '(forall ((?t Number)) (HoldsAt (position P3 track2 3) ?t)))


  ;; these come from perception
  (assert '(not (Clipped 0 (onrails trolley track1) 1)))
  (assert '(not (Clipped 0 (onrails trolley track1) 2)))
  (assert '(not (Clipped 1 (onrails trolley track1) 1)))
  
  (assert '(Clipped 2 (onrails trolley track1) 3 ))
  (assert '(Clipped 2 (onrails trolley track1) 4 ))
  (assert '(Clipped 2 (onrails trolley track1) 5 ))
  (assert '(not (exists ((?a Agent) (?t Number))
		 (Happens (action ?a (switch track2 track1)) ?t))))
  (assert '(forall ((?t2 Number))
	    (not  (Clipped 2 (onrails trolley track2) ?t2))))
  (assert '(Happens (action I (switch track1 track2)) 2)))


(defun run-scenario-1-means ()
  (scenario-1-means)
  (prove `(forall ((?t Number)) (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P1) ?t)))))
  (prove `(forall ((?t Number)) (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P2) ?t)))))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Scenario 2 Base ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun scenario-2-base ()

  (setup)
  
  (assert '(forall ((?t Number) (?track Track) (?p Number))
	    (not (HoldsAt (position P3 ?track ?p) ?t))))

  ;;; These common from perception. 
  (assert '(not (Clipped 0 (onrails trolley track1) 1)))
  (assert '(not (Clipped 0 (onrails trolley track1) 2)))
  (assert '(not (Clipped 0 (onrails trolley track1) 3)))
  (assert '(not (Clipped 0 (onrails trolley track1) 4)))
  (assert '(not (Clipped 0 (onrails trolley track1) 5)))
  (assert '(not (Clipped 0 (onrails trolley track1) 6))))

(defun run-scenario-2-base ()

  (print "Base")
  (scenario-1-base)
  (if (equalp :PROOF-FOUND (prove `(exists ((?t Number))  (HoldsAt (dead P1) ?t))))
      (cprint "P1 Dead")
      (cprint "P1 Alive"))
  
   (scenario-1-base)
   (if (equalp :PROOF-FOUND (prove `(exists ((?t Number)) (HoldsAt (dead P2) ?t))))
      (cprint "P2 Dead")
      (cprint "P2 Alive"))
   
   (scenario-1-base)
   (if (equalp :PROOF-FOUND (prove `(forall ((?t Number)) (implies (Prior ?t ,*horizon*) (not (HoldsAt (dead P3) ?t))))))
       (cprint "P3 Alive")
       (cprint "P3 Dead"))
   
   nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Scenario 2 Action ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scenario-2-action ()

  (setup)
  
  (assert '(Heavy P3))
  
  (assert '(Happens (action I (drop P3 track1 3)) 1 ))
  (assert '(not (Clipped 0 (onrails trolley track1) 1)))
  (assert '(not (Clipped 0 (onrails trolley track1) 2)))
  (assert '(not (Clipped 0 (onrails trolley track1) 3)))

  (assert '(forall ((?a Agent) (?trolley Trolley) (?track Track) (?position Number) (?time Number))
	    (implies (and 
		      (Heavy ?a)
		      (HoldsAt (position ?a ?track ?position) ?time)
		      (HoldsAt (position ?trolley ?track ?position) ?time))
	     (and
	      (Happens (damage ?trolley) ?time)
	      (forall ((?tt Number)) (implies (Prior ?time ?tt)
					       (Clipped 0 (onrails trolley track1) ?tt))))))))


(defun run-scenario-2-action ()
  (run-scenario #'scenario-2-action "Scenario 2: Push P3"))


(defun scenario-2-means ()

  (setup)
  
  ;(assert '(Heavy P3))
  
 ; (assert '(Happens (action I (drop P3 track1 3)) 1 ))
  (assert '(not (Clipped 0 (onrails trolley track1) 1)))
  (assert '(not (Clipped 0 (onrails trolley track1) 2)))
  (assert '(not (Clipped 0 (onrails trolley track1) 3)))
  (assert '(not (Clipped 0 (onrails trolley track1) 4)))
  (assert '(not (Clipped 0 (onrails trolley track1) 5)))
  (assert '(not (Clipped 0 (onrails trolley track1) 6)))

  (assert '(forall ((?a Agent) (?trolley Trolley) (?track Track) (?position Number) (?time Number))
	    (implies (and 
		      (Heavy ?a)
		      (HoldsAt (position ?a ?track ?position) ?time)
		      (HoldsAt (position ?trolley ?track ?position) ?time))
	     (and
	      (Happens (damage ?trolley) ?time)
	      (forall ((?tt Number)) (implies (Prior ?time ?tt)
					       (Clipped 0 (onrails trolley track1) ?tt))))))))

(defun run-scenario-2-means ()
  (scenario-2-means)
  (prove '(exists ((?t Number)) (HoldsAt (dead P1) ?t)))
  (prove '(exists ((?t Number)) (HoldsAt (dead P2) ?t))))

(defun sanity-check (setup name)
  (print (concatenate 'string "Sanity checks for " name))
  (funcall setup)
  (let ((ans  (prove '(and Z (not Z)))))
    (if (equalp ans :PROOF-FOUND)
	(print "  FAILED")
	(print "  PASSED"))))


(defun sanity-checks ()
  
  (sanity-check #'scenario-1-base "Scenario 1 Do Nothing")
  (sanity-check #'scenario-1-action "Scenario 1 Switch the Trolley")
  
  (sanity-check #'scenario-2-base "Scenario 2 Do Nothing")
  (sanity-check #'scenario-2-action "Scenario 2 Push Away")
  (values))



(defun all-simulations ()
  
   (time (run-scenario-1-base))
   (force-output)
   (time (run-scenario-1-action))
   (force-output)
   (time (run-scenario-2-base))
   (force-output)
   (time (run-scenario-2-action))

   (values))
