;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 1"
 :description ""
 :assumptions {1 P}
 :goal P}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 2"
 :description ""
 :assumptions {1 (or P Q)}
 :goal (pos P)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 3"
 :description ""
 :assumptions {1 (or P Q)}
 :goal (pos P)}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 4"
 :description ""
 :assumptions {1 (or p q)
               2 (if (pos p) (and u (or a b)))}
 :goal (pos a)}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 5"
 :description ""
 :assumptions {1 (or p q)
               2 (if (pos p) (and u (or a (or b c))))}
 :goal (pos c)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 6"
 :description ""
 :assumptions {1 (or p q)
               2 (if (pos p) (and u (or a (or b c))))}
 :goal (pos c)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 7"
 :description ""
 :assumptions {}
 :goal (or p (not p) (pos q))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 8"
 :description ""
 :assumptions {}
 :goal (or (or p (not p)) (pos q))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "test 9"
 :description ""
 :assumptions {}
 :goal (or (or p (not p)) (pos (and p (not p))))}

