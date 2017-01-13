
(defparameter *A1* (and (House h1) (House h2) (House h3)
			(not (or (= h1 h2) (= h2 h3) (= h3 1)))))

(defparameter *pets-are-different* (not
				    (or (= (pet h1) (pet h2))
					(= (pet h2) (pet h3)) 
					(= (pet h3) (pet h1)))))


(defparameter *drinks-are-different* (not
				      (or (= (drink h1) (drink h2))
					  (= (drink h2) (drink h3)) 
					  (= (drink h3) (drink h1)))))


(defparameter *persons-are-different* (not
				      (or (= (person h1) (person h2))
					  (= (person h2) (person h3)) 
					  (= (person h3) (person h1)))))

(defparameter *p1* (= (drink h3) milk))

(defparameter *p2* (owns spaniard dog))

(defparameter *p3* (drinks ukranian tea))

(defparameter *p4* (= norwegian (person h1)))

(defparameter *p5* (next-to (house-of norwegian) (house-of ukranian)))
