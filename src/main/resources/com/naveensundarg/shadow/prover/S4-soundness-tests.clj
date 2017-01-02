;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-1"
 :description ""
 :assumptions {1 Q}
 :goal P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name "S5 schema."
 :description ""
 :assumptions {}
 :goal (if
         (pos P)
         (nec (pos P)))}