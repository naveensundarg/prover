;; Examples from  Section 2.4 in http://people.csail.mit.edu/kostas/dpls/ndlZero.pdf

{:name        "test 1"
 :assumptions {}
 :input       (assume P :in (suppose-absurd (not P) :in (!absurd P (not P))))
 :output      (implies P (not (not P)))}

{:name        "test 2"
 :assumptions {}
 :input       (assume (implies P Q) :in
                      (assume (implies Q R) :in
                              (assume P :in
                                     (!modus-ponens
                                       (implies Q R)
                                       (!modus-ponens (implies P Q) P )))))
 :output      (implies (implies P Q)
                       (implies (implies Q R)
                                (implies P R)))}

{:name        "test 3"
 :assumptions {}
 :input       (assume (implies P (implies Q R)) :in
                      (assume (and P Q) :in
                              ))
 :output      (implies (implies P (implies Q R))
                       (implies (and P Q)
                                R))}













{:name        "test 1"
 :assumptions {}
 :input       P
 :output      P}

{:name        "test 2"
 :assumptions {1 P
               2 Q}
 :input       (!both P Q)
 :output      (and P Q)}

{:name        "test 3"
 :assumptions {}
 :input       (assume (and P Q) :in
                      (!both (!right-and (and P Q))
                              (!left-and (and P Q))))
 :output      (implies (and P Q ) (and Q P ))}




