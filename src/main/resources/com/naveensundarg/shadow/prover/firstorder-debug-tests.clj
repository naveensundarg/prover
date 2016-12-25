

{:name        "traid zoo reflexivity"
 :description ""
 :assumptions {AX1 (forall (?x) (or (Camel ?x) (Llama ?x) (Aard ?x)))
               AX2 (not (exists (?x) (and (Camel ?x) (Aard ?x))))
               AX3 (not (exists (?x) (and (Aard ?x) (Llama ?x))))
               AX4 (not (exists (?x) (and (Llama ?x) (Camel ?x))))
               AX5 (forall (?x ?y) (if (and (Camel ?x) (Camel ?y)) (SameSpecies ?x ?y)))
               AX6 (forall (?x ?y) (if (and (Llama ?x) (Llama ?y)) (SameSpecies ?x ?y)))
               AX7 (forall (?x ?y) (if (and (Aard ?x) (Aard ?y)) (SameSpecies ?x ?y)))
               AX8 (forall (?x ?y) (if (and (SameSpecies ?x ?y) (Camel ?x)) (Camel ?y)))
               AX9 (forall (?x ?y) (if (and (SameSpecies ?x ?y) (Llama ?x)) (Llama ?y)))
               AX10 (forall (?x ?y) (if (and (SameSpecies ?x ?y) (Aard ?x)) (Aard ?y)))

               }
 :goal        (forall (?x)  (SameSpecies ?x ?x))}


;{:name "false-belief-task-M2"
; :description "bug"
; :assumptions {                                             ;1 (Common! (forall (?t) (initiates e f ?t)))
;               ;2 (Knows! a1 (Knows! a2 (happens e t1)))
;               ;3 (Common! (< t1 t2))
;               A (forall (?x)
;                          (iff (P ?x)
;                               (exists (?x) (and (Q ?x) R) )))}
;
;:goal (iff (P a) (exists (?x) (and (Q ?x) R) )) }
;
;
