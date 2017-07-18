
{:name        "I know an infinite number of things"
 :description "I know for example from PA that 27 times zero is zero,
               and therefore I know this disjoined with any declarative proposition P;
               and therefore I know that disjunction disjoined with any proposition P; ad infinitum."

 :assumptions {1 (Knows! I (if PA (= 0 (multiply 27 0))))}

 :goal  (forall [?Q] (Knows! I (or (if PA (= 0 (multiply 27 0))) ?Q)) )     }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Knowability paradox"
 :description " "

 :assumptions {}
 :goal (forall [?P] (forall [?agent] (not (pos (Knows! ?agent (and ?P (not (Knows! ?agent ?P))))))))}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:name        "Knowability paradox"
 :description " \exists p  ~\Diamond \exists x Kx (Tp & ~ \exist y Ky Tp)"

 :assumptions {}
 :goal (exists [?P] (not (pos (exists [?x] (Knows! ?x (and ?P (not (exists [?y] (Knows! ?y ?P)))))))))}





