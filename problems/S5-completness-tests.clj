;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "*prop-nd-true-test-1"
 :description ""
 :assumptions {1 P}
 :goal P}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "modalized modus ponens"
 :description ""
 :assumptions {1 (nec (if P Q))
               2 (nec P)}
 :goal (nec Q)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name "relative of mt"
 :description ""
 :assumptions {1 (nec (if P Q))
               2 (not Q)}
 :goal (not P)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "L-P_or_Q-LnotP_impliesLQ."
 :description ""
 :assumptions {1 (nec (or P Q))
               2 (nec (not P))}
 :goal (nec Q)}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "Lp_and_Mp_then_M-p_and_q."
 :description ""
 :assumptions {}
 :goal (if
         (and (nec P) (pos Q))
         (pos (and P Q)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "notMp_then_notLp."
 :description ""
 :assumptions {}
 :goal (if
         (not (pos P))
         (not (nec P)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "Mp_and_notMq_then_M-p_or_q."
 :description ""
 :assumptions {}
 :goal (if
         (and (pos P) (not (pos Q)) )
         (pos (or P Q)))}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name "S4 schema."
 :description ""
 :assumptions {}
 :goal (if
         (nec P)
         (nec (nec P)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
{:name "S5 schema."
 :description ""
 :assumptions {}
 :goal (if
         (pos P)
         (nec (pos P)))}