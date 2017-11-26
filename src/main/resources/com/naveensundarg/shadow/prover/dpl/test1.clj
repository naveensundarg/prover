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


{:name        "test 4"
 :assumptions {}
 :input       (assume P :in (suppose-absurd (not P) :in (!absurd P (not P))))
 :output      (implies P (not (not P)))}


