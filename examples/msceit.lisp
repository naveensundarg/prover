(in-package :snark-user)

(load "/Users/naveensundarg/projects/prover/snark-20120808r02/snark-interface.lisp")


(defun msceit-example-axioms ()
  (assert '(= t2 (next t1)))
  (assert '(= t3 (next t2)))

  (assert '(= stressed (aggravated anxious)))
  (assert '(= overwhelmed (aggravated stressed)))
  
  (assert '(ClassOf stress-emotion anxious))
  (assert '(ClassOf stress-emotion stressed))
  (assert '(ClassOf stress-emotion overwhelmed))

  (assert '(forall (?a) (aggravator stress-emotion (work ?a))))


  (assert '(forall (?a ?e1 ?e2 ?t1 ?t2 ?event) 
	    (implies
	     (and (holds (aff ?a ?e1) ?t1)
	      (= ?t2 (next ?t1))
	      (= ?e2 (aggravated ?e1))
	      (happens ?event ?t1)
	      (aggravator ?c ?evet)
	      (ClassOf ?c ?e1))
	     (holds (aff ?a ?e2) ?t2))))
  
  (assert '(holds (aff Tim anxious) t1))
  (assert '(happens (work Tim) t1))

  )


(defun setup-msceit ()
  (setup-snark)
  (msceit-example-axioms)
  )
