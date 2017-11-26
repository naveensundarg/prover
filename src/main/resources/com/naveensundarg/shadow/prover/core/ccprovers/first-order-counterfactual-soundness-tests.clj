 {:name "Counterfactual 3"
 :description ""
 :assumptions {A1 (forall [?x]
                     (if (GoToDoctor ?x) (not (Sick ?x))))
               A2 (not (GoToDoctor john))}

 :goal (=> (GoToDoctor john) (Rich john))}



{:name "Counterfactual Mortality 3"
 :description ""
 :assumptions {A1 (forall [?x] (if (Human ?x) (Mortal ?x)))
               A2 (Human socrates)}

 :goal (=> (not (Mortal socrates)) (and P (not P)))}



 {:name "Counterfactual Identity 3"
 :description ""
 :assumptions {A1 (forall [?x] (if (Rich ?x) (CanAffordLuxury ?x)))
               A2 (Rich jack)
               A3 (not (Rich jim))}

 :goal (=> (= jack jim) (Cat jim))}


