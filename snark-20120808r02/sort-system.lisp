

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



(defun )
