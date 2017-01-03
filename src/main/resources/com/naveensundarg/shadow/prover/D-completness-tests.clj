;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-1"
 :description ""
 :assumptions {1 P}
 :goal P}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "a"
 :description ""
 :assumptions {}
 :goal (if (nec (nec P)) (nec (pos P)))}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "b"
 :description ""
 :assumptions {}
 :goal (if (nec (nec P)) (pos (pos P)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "c"
 :description ""
 :assumptions {}
 :goal (if (nec P) (pos (if Q P)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "d"
 :description ""
 :assumptions {}
 :goal (if (and (nec P) (nec (if P Q)))  (pos Q))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "e"
 :description ""
 :assumptions {}
 :goal (not (and (nec P) (nec (not P))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "f"
 :description ""
 :assumptions {}
 :goal (not (and (nec (and P Q)) (nec (if P (not Q)))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "g"
 :description ""
 :assumptions {}
 :goal (or (or (pos (not P)) (pos (not Q)))
           (pos (and P Q)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "Chisholm's paradox"
 :description ""
 :assumptions {1 (nec goes_assist_neighbors)
               2 (nec (if goes_assist_neighbors tells_coming))
               3 (if (not goes_assist_neighbors)
                   (nec (not tells_coming)))
               4 (not goes_assist_neighbors)}
 :goal (and P (not P)) }


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "hw3"
 :description ""
 :assumptions {}
 :goal (if (nec phi) (pos phi))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "hw4"
 :description ""
 :assumptions {}
 :goal (if (pos phi) (pos (or phi psi)))}

