"begin"
"assumption" (Knows! a1 t (happens (action a2 alpha) t))
"assumption" (Common! now (forall (?a ?d ?t) (if (happens (action ?a ?d) ?t) (knows ?a ?t (happens (action ?a ?d) ?t)))))
"assumption" (Common! now (if (happens (action a2 alpha) t) (knows a2 t (happens (action a alpha) t))))
"goal"
"end"