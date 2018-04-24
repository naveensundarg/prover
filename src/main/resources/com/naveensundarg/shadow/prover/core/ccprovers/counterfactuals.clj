
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual 1"

 :description "If John goes to the doctor, he will not be sick.
               John did not go to the doctor.
               If John had gone to the doctor, he would not have been sick."

 :assumptions {A1 (forall [?x]
                          (if (GoToDoctor ?x) (not (Sick ?x))))
               A2 (not (GoToDoctor john))}

 :goal        (=> (GoToDoctor john) (not (Sick john)))}

{:name        "Counterfactual 2"

 :description "If John goes to the doctor, he will not be sick.
               John did not go to the doctor.
               If John had gone to the doctor, he would not have been sick."

 :assumptions {A1 (forall [?x]
                          (if (GoToDoctor ?x) (not (Sick ?x))))
               A2 (not (GoToDoctor john))}

 :goal        (if (GoToDoctor john) (and P (not P)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "Counterfactual Mortality 1"

 :description "Socrates is human. If Socrates is not mortal, then he is not human."

 :assumptions {A1 (forall [?x] (if (Human ?x) (Mortal ?x)))
               A2 (Human socrates)}

 :goal        (=> (not (Mortal socrates)) (not (Human socrates)))}


{:name        "Counterfactual Mortality 2"

 :description "Socrates is human. If Socrates is not mortal, then he is not human."

 :assumptions {A1 (forall [?x] (if (Human ?x) (Mortal ?x)))
               A2 (Human socrates)}

 :goal        (if (not (Mortal socrates)) (and P (not P)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Identity A 1"

 :description "Jim is rich. Jack is poor. If Jack were Jim, then he can afford luxury."

 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))}

 :goal        (=> (= jack jim) (CanAffordLuxury jim))}



{:name        "Counterfactual Identity A 2"

 :description "Jim is rich. Jack is poor. If Jack were Jim, then he can afford luxury."

 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))}

 :goal        (if (= jack jim) (and P (not P)))}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Identity B 1"

 :description "Jim is rich. Jack is poor. Jack and Jim are different.
               If Jack were Jim, then he can afford luxury."

 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))
               A4 (not (= jim jack))}

 :goal        (=> (= jack jim) (CanAffordLuxury jim))}



{:name        "Counterfactual Identity B 2"

 :description "Jim is rich. Jack is poor. Jack and Jim are different.
               If Jack were Jim, then he can afford luxury."

 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))
               A4 (not (= jim jack))}

 :goal        (if (= jack jim) (and P (not P)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Disjunction 1"

 :description "Everything is big or small. That tree is big. If that tree is not big, then it is small."

 :assumptions {A1 (forall [?x] (or (Big ?x) (Small ?x)))
               A2 (Big tree)}

 :goal        (=> (not (Big tree)) (Small tree))}


{:name        "Counterfactual Disjunction 2"

 :description "Everything is big or small. That tree is big. If that tree is not big, then it is small."

 :assumptions {A1 (forall [?x] (or (Big ?x) (Small ?x)))
               A2 (Big tree)}

 :goal        (if (not (Big tree)) (and P (not P)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Disjunction A 1"

 :description "Everything is a human or an animal. j is a human. All humans think.
               If j does not think, then either j is an animal or there is a human that does not think."

 :assumptions {A1 (forall [?x] (or (Human ?x) (Animal ?x)))
               A2 (Human j)
               A3 (forall [?x] (if (Human ?x) (Thinks ?x)))}

 :goal        (=> (not (Thinks j)) (or (Animal j)
                                       (exists [?x] (and (Human ?x) (not (Thinks ?x))))))}


{:name        "Counterfactual Disjunction A 2"

 :description "Everything is a human or an animal. j is a human. All humans think.
               If j does not think, then either j is an animal or there is a human that does not think."

 :assumptions {A1 (forall [?x] (or (Human ?x) (Animal ?x)))
               A2 (Human j)
               A3 (forall [?x] (if (Human ?x) (Thinks ?x)))}

 :goal        (if (not (Thinks j)) (and P (not P)))}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Linking A 1"

 :description "Counterfactual reasoning over long links."

 :assumptions {A1 (if P Q)
               A2 (if Q R)
               A3 (if R S)
               A4 (not P)}

 :goal        (=> P R)}


{:name        "Counterfactual Linking A 2"

 :description "Counterfactual reasoning over long links."

 :assumptions {A1 (if P Q)
               A2 (if Q R)
               A3 (if R S)
               A4 (not P)}

 :goal        (if P (and P (not P)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Reverse Linking A 1"

 :description "Counterfactual reasoning over long links."

 :assumptions {A1 (if P Q)
               A2 (if Q R)
               A3 (if R S)
               A4 S}

 :goal        (=> (not S) (not P))}


{:name        "Counterfactual Reverse Linking A 2"

 :description "Counterfactual reasoning over long links."

 :assumptions {A1 (if P Q)
               A2 (if Q R)
               A3 (if R S)
               A4 S}

 :goal        (if (not S) (and P (not P)))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Reverse Linking B 1"

 :description "Counterfactual reasoning over long links with quantifiers."

 :assumptions {A1 (if (forall [?x] (P ?x)) Q)
               A2 (if Q R)
               A3 (if R S)
               A4 S}

 :goal        (=> (not S) (exists [?x] (not (P ?x))))}


{:name        "Counterfactual Reverse Linking B 2"

 :description "Counterfactual reasoning over long links with quantifiers."

 :assumptions {A1 (if (forall [?x] (P ?x)) Q)
               A2 (if Q R)
               A3 (if R S)
               A4 S}

 :goal        (if (not S) (and P (not P)))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Infinite Domain"

 :description "Counterfactual reasoning over infinite domains."

 :assumptions {A1 (forall [?x] (exists [?y] (Bigger ?y ?x)))
               A2 (forall [?x ?y] (not (and (Bigger ?x ?y) (Bigger ?y ?x))))
               A3 (forall [?x ?y ?z] (if (and (Bigger ?x ?y) (Bigger ?y ?z)) (Bigger ?x ?z)))}

 :goal        (=>   (exists [?x] (not (P ?x))))}


{:name        "Infinite Domain"

 :description "Counterfactual reasoning over infinite domains."

 :assumptions {A1 (forall [?x] (exists [?y] (Bigger ?y ?x)))
               A2 (forall [?x ?y] (not (and (Bigger ?x ?y) (Bigger ?y ?x))))
               A3 (forall [?x ?y ?z] (if (and (Bigger ?x ?y) (Bigger ?y ?z)) (Bigger ?x ?z)))}



 :goal        (if (not S) (and P (not P)))}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{
 :name        "Counterfactual modal 1"
 :description ""
 :assumptions {
               A1 (Believes! a (forall [?x]
                                       (if (GoToDoctor ?x) (not (Sick ?x)))))
               A2 (Believes! a (not (GoToDoctor john)))}

 :goal        (Believes! a (=> (GoToDoctor john) (not (Sick john))))
 }


{
 :name        "Counterfactual modal 2"
 :description ""
 :assumptions {
               A1 (Believes! a (forall [?x]
                                       (if (GoToDoctor ?x) (not (Sick ?x)))))
               A2 (Believes! a (not (GoToDoctor john)))}

 :goal        (Believes! a (if (GoToDoctor john) (and P (not P))))
 }


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Mortality 1"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (if (Human ?x) (Mortal ?x))))
               A2 (Believes! a now (Human socrates))}

 :goal        (Believes! a now (=> (not (Mortal socrates)) (not (Human socrates))))}


{:name        "Counterfactual Mortality 2"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (if (Human ?x) (Mortal ?x))))
               A2 (Believes! a now (Human socrates))}

 :goal        (Believes! a now (if (not (Mortal socrates)) (and P (not P))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Identity A  1"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x))))
               A2 (Believes! a now (Rich jack))
               A3 (Believes! a now (not (Rich jim)))}

 :goal        (Believes! a now (=> (= jack jim) (CanAffordLuxury jim)))}



{:name        "Counterfactual Identity A 2"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x))))
               A2 (Believes! a now (Rich jack))
               A3 (Believes! a now (not (Rich jim)))}

 :goal        (Believes! a now (if (= jack jim) (and P (not P))))}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "Counterfactual Disjunction 1"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (or (Big ?x) (Small ?x))))
               A2 (Believes! a now (Big tree))}

 :goal        (Believes! a now (=> (not (Big tree)) (Small tree)))}


{:name        "Counterfactual Disjunction 2"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (or (Big ?x) (Small ?x))))
               A2 (Believes! a now (Big tree))}

 :goal        (Believes! a now (if (not (Big tree)) (and P (not P))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{:name        "Counterfactual Disjunction A 1"
 :description ""
 :assumptions {A1 (Believes! a now (Believes! b now (forall [?x] (or (Big ?x) (Small ?x)))))
               A2 (Believes! a now (Believes! b now (Big tree)))}

 :goal        (Believes! a now (Believes! b now (=> (not (Big tree)) (Small tree))))}


{:name        "Counterfactual Disjunction A 2"
 :description ""
 :assumptions {A1 (Believes! a now (Believes! b now (forall [?x] (or (Big ?x) (Small ?x)))))
               A2 (Believes! a now (Believes! b now (Big tree)))}

 :goal        (Believes! a now (Believes! b now (if (not (Big tree)) (and P (not P)))))}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Counterfactual Disjunction Ab 1"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (or (Human ?x) (Animal ?x))))
               A2 (Believes! a now (Human j))
               A3 (Believes! a now (forall [?x] (if (Human ?x) (Thinks ?x))))}

 :goal        (Believes! a now (=> (not (Thinks j)) (or (Animal j)
                                                        (exists [?x] (and (Human ?x) (not (Thinks ?x)))))))}


{:name        "Counterfactual Disjunction Ab 2"
 :description ""
 :assumptions {A1 (Believes! a now (forall [?x] (or (Human ?x) (Animal ?x))))
               A2 (Believes! a now (Human j))
               A3 (Believes! a now (forall [?x] (if (Human ?x) (Thinks ?x))))}

 :goal        (Believes! a now (if (not (Thinks j)) (and P (not P))))}

