;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name       "*prop-nd-false-test-1*"
 :description ""
 :assumptions {}
 :goal        P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-2*"
 :description ""
 :assumptions {1 (or p q)}
 :goal        P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-3*"
 :description ""
 :assumptions {1 (and p q)}
 :goal        r}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-4*"
 :description ""
 :assumptions {1 (if p q)}
 :goal        p}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-5*"
 :description ""
 :assumptions {1 (if p q)}
 :goal        q}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-6*"
 :description ""
 :assumptions {1 (if p q)}
 :goal        (not (or p (not q)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-7*"
 :description ""
 :assumptions {1 (if p q)
               2 (not (not (or p (not q))))}
 :goal        (not (or p (not q)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-8*"
 :description ""
 :assumptions {}
 :goal        (and p (not p))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-9*"
 :description ""
 :assumptions {}
 :goal        (not (not (and p (not P))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-10*"
 :description ""
 :assumptions {}
 :goal        (if p (not p))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
