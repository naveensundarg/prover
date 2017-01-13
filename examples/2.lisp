
(in-package :snark-user)
(defparameter *P1* '(forall (?x ?y)	
			   (implies (and (Snail ?x) (Bird ?y))
				    (SmallerThan ?x ?y))))

(defparameter *P2* '(forall (?x ?y)	
			   (implies (and (Bird ?x) (Fox ?y))
				    (SmallerThan ?x ?y))))


(defparameter *P3* '(forall (?x ?y)	
			   (implies (and (Fox ?x) (Wolf ?y))
				    (SmallerThan ?x ?y))))



(defparameter *P4* '(forall (?w ?f)
			     (implies (and (Wolf ?w) (Fox ?f))
				      (not (Eats ?w ?f)))))

(defparameter *P5* '(forall (?w ?g)
			     (implies (and (Wolf ?w) (Grain ?g))
				      (not (Eats ?w ?g)))))



(defparameter *P6* '(forall (?b ?c)
			     (implies (and (Bird ?b) (Caterpillar ?c))
				      (Eats ?b ?c))))


(defparameter *P7* '(forall (?b ?s)
			     (implies (and (Bird ?b) (Snail ?s))
				      (not (e ?b ?s)))))


(defparameter *P8* '(forall (?c)
			   (implies (Caterpillar ?c)
				    
			(exists (?p) (and (Plant ?p) (Eats ?c ?p))))))

(defparameter *P9* '(forall (?s)
			   (implies (Snail ?s)
				     (exists (?p)
					     (and (Plant ?p)
						  (Eats ?s ?p))))))

(defparameter *P10* '(forall (?p1 ?p2 ?a1 ?a2)
			    (implies (and  (Plant ?p1) (Plant ?p2))
				     (implied-by (or (Eats ?a1 ?a2) (Eats ?a1 ?p1)) 
						 (and (SmallerThan ?a2 ?a1) (Eats ?a2 ?p2))))))

(defparameter *P11* '(forall (?x) (iff (Grain ?y)(Plant ?x)) ))
