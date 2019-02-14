;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-1"
 :description ""
 :assumptions {1 P}
 :goal P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-1a"
 :description ""
 :assumptions {1 P}
 :goal (or P Q)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-2"
 :description ""
 :assumptions {1 P
               2 Q}
 :goal P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-3*"
 :description ""
 :assumptions {}
 :goal (if P P)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "case prop-nd-true-test-4*"
 :description ""
 :assumptions {}
 :goal (if P (if Q P))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-5*"
 :description ""
 :assumptions {1 P
               2 Q}
 :goal (and P Q )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-6*"
 :description ""
 :assumptions {1 P
               2 Q
               3 R}
 :goal (and (and R Q ) R )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-7*"
 :description ""
 :assumptions {1 P
               2 Q
               3 R}
 :goal (and (and R Q ) R )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-8*"
 :description ""
 :assumptions {1 Q
               2 R}
 :goal (if P (and R (and Q P ) ))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-9*"
 :description ""
 :assumptions {1 P
               2 Q}
 :goal P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-10*"
 :description ""
 :assumptions {1 (and P (and Q R ) )
               2 U
               3 V}
 :goal (and R (and U V ) )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-11*"
 :description ""
 :assumptions {1 (or P Q )}
 :goal (or Q P )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-12*"
 :description ""
 :assumptions {1 (or P (or Q R ) )}
 :goal (or (or R Q ) P )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-13*"
 :description ""
 :assumptions {1 (not (or P Q ))}
 :goal (not P)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-14*"
 :description ""
 :assumptions {1 (not (or P Q ))}
 :goal (and (not P) (not Q) )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-15*"
 :description ""
 :assumptions {1 (and (not P) (not Q))}
 :goal (not (or P Q ))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-16*"
 :description ""
 :assumptions {1 (not (or (not P) Q ))}
 :goal P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-17* kok_o213_8_32"
 :description ""
 :assumptions {1 (if H (and E D ))
               2 (if (or E My ) R)
               3 (if Ma (not R))}
 :goal (if H (not Ma))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-18* kok_o213_8_35"
 :description ""
 :assumptions {1 (if Small_c (or Small_d Small_e ))
               2 (if (not Cube_b) Small_b)
               3 (if Small_d (not Small_c))
               4 (if Cube_b (not Small_e))}
 :goal (if Small_c Small_b)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-19* kok_o213_8_35"
 :description ""
 :assumptions {1 (if Large_c Tet_c)
               2 (and Small_a (or Medium_b Large_c ) )
               3 (if Medium_b Front_Of_a_b)}
 :goal (if (not Tet_c) Front_Of_a_b)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-20*"
 :description ""
 :assumptions {}
 :goal (iff (and A (not B) ) (not (if A B)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-21*"
 :description ""
 :assumptions {}
 :goal (or P (not P) )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-22*"
 :description ""
 :assumptions {}
 :goal (if P (if Q P))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-23* 1"
 :description ""
 :assumptions {}
 :goal (if (not (if p q)) (if q p))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-24* 2"
 :description ""
 :assumptions {}
 :goal (iff (if p q) (if (not q) (not p)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-25* 3"
 :description ""
 :assumptions {}
 :goal (if (not (if p q)) (if q p))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-26* 4"
 :description ""
 :assumptions {}
 :goal (iff (if (not p) q) (if (not q) p))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-27* 5"
 :description ""
 :assumptions {}
 :goal (if (if (or p q ) (or p r )) (or p (if q r) ))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-28* 7"
 :description ""
 :assumptions {}
 :goal (or P (not (not (not P))) )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-29* 8 (Peirce's Law)"
 :description ""
 :assumptions {}
 :goal (if (if (if p q) p) p)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-30* Problem 9"
 :description ""
 :assumptions {}
 :goal (if (and (and (or p q ) (or (not p) q ) ) (or p (not q) ) )
         (not (or (not p) (not q) )))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-31* Problem 10"
 :description ""
 :assumptions {1 (if r (and p q ))
               2 (if q r)
               3 (if p (or q r))}
 :goal (if p q)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-32* Problem 11"
 :description ""
 :assumptions {}
 :goal (if p p)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;{:name "*prop-nd-true-test-33* Problem 12"
; :description ""
; :assumptions
; {}
; :goal (iff (iff (iff p q) r) (iff p (iff q r)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-34* Problem 13a"
 :description ""
 :assumptions {}
 :goal (if (or p (and q r ) ) (and (or p q ) (or p r ) ))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-34* Problem 13b"
 :description ""
 :assumptions {}
 :goal (if (and (or p q ) (or p r ) ) (or p (and q r ) ))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-35*"
 :description ""
 :assumptions {}
 :goal (if (if p (not p)) (if p q))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-36* testing chaining"
 :description ""
 :assumptions {1 p
               2 (if e (and q (and r s ) ))
               3 (if p (and w e ))}
 :goal r}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-36* testing chaining with a theorem at the start"
 :description ""
 :assumptions {1 (if (or p (not p) ) (and w e ))
               2 (if e (and q (and r s ) ))}
 :goal r}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-38* complex instantiation of a simple theorem"
 :description ""
 :assumptions {}
 :goal (or (if p (if q r)) (not (if p (if q r))) )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-40* Negation of a contradiction"
 :description ""
 :assumptions {}
 :goal (not (and p (not p) ))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-41* Negation of a theorem leads to explosion"
 :description ""
 :assumptions {1 (not (or p (not p) ))}
 :goal z}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-42* Negation of a theorem leads to explosion"
 :description ""
 :assumptions {1 (not (or p (not p)))}
 :goal (or p q )}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-43*"
 :description ""
 :assumptions {}
 :goal (if (if (if P Q) R) (if P (if Q R)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-44*"
 :description ""
 :assumptions {}
 :goal (iff (iff P Q) (iff Q P))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-45* Biconditional chaining"
 :description ""
 :assumptions {1 (iff q (and a b ))
               2 (iff p (and q r ))}
 :goal (if p a)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;{:name "*prop-nd-true-test-46* direction 1 of 33"
; :description ""
; :assumptions {}
; :goal (if (iff (iff p q) r) (iff p (iff q r)))}
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-simple-proof-by-cases-1*"
 :description ""
 :assumptions {1 (or (and P Q) (and P R))}
 :goal  P}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-simple-proof-by-cases-1*"
 :description ""
 :assumptions {1 (or (or  P Q) R )}
 :goal (or P (or Q R))}


