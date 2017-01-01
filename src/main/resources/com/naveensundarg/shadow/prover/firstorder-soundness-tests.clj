;;; these should not be proven!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "*prop-nd-false-test-1*"
 :description "Can't prove an atom from no premises"
 :assumptions {}
 :goal        P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description "Case sensitivity check"
 :assumptions {1 (or p q)}
 :goal        P}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {1 (and p q)}
 :goal        r}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {1 (if p q)}
 :goal        p}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {1 (if p q)}
 :goal        q}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {1 (if p q)}
 :goal        (not (or p (not q)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {1 (if p q)
               2 (not (not (or p (not q))))}
 :goal        (not (or p (not q)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {}
 :goal        (and p (not p))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {}
 :goal        (not (not (and p (not P))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {}
 :goal        (if p (not p))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description "existential implies universal"
 :assumptions {}
 :goal        (if (exists (?x)  (P ?x)) (forall (?y)  (P ?y)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        ""
 :description ""
 :assumptions {}
 :goal        (if (forall (?x) (exists (?y)  (Loves ?x, ?y)))
                (exists (?y) (forall (?x)  (Loves ?x, ?y))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


