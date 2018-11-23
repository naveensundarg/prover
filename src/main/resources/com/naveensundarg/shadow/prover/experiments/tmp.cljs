{
  :name \"countermasquerading on agents\"
  :description
  \"with defunctionalized equality predicates and a modified form of Leibnitz's law,
  this will experiment if we can tell agents apart by their unequal properties.\"
  :assumptions {
                 1
                 (forall (?n ?a ?g ?j)
                         (and
                          (and
                           (= (eval_proj name (agent_con ?n ?a ?g ?j)) ?n)
                           (= (eval_proj age  (agent_con ?n ?a ?g ?j)) ?a))
                          (and
                           (= (eval_proj gender (agent_con ?n ?a ?g ?j) ?g))
                           (= (eval_proj joined (agent_con ?n ?a ?g ?j) ?j)))))
                 2
                 (forall (?s ?i ?agent ?proj)
                         (and
                          (iff
                           (eval_pred (eq_s ?s ?proj) ?agent)
                           (and
                            (or (= ?proj name) (= ?proj gender))
                            (= ?s (eval_proj ?proj ?agent))))
                          (iff
                           (eval_pred (eq_i ?i ?proj) ?agent)
                           (and
                            (or (= ?proj age) (= ?proj joined))
                            (= ?i (eval_proj ?proj ?agent))))))
                 3
                 (forall (?ax ?ay)
                         (if (= ?ax ?ay)
                           (forall (?proj_s ?proj_i ?s ?i)
                                   (and
                                    (iff
                                     (eval_pred (eq_s ?s ?proj_s) ?ax)
                                     (eval_pred (eq_s ?s ?proj_s) ?ay))
                                    (iff
                                     (eval_pred (eq_i ?i ?proj_i) ?ax)
                                     (eval_pred (eq_i ?i ?proj_i) ?ay))))))
                 4
                 (forall (?proj_s1 ?proj_s2 ?proj_i1 ?proj_i2 ?s1 ?s2 ?i1 ?i2)
                         (and
                          (and
                           (incompat (eq_s ?s1 ?proj_s1) (eq_i ?i1 ?proj_i1))
                           (incompat (eq_i ?i1 ?proj_i1) (eq_s ?s1 ?proj_s1)))
                          (and
                           (iff
                            (incompat (eq_s ?s1 ?proj_s1) (eq_s ?s2 ?proj_s2))
                            (or (not (= ?s1 ?s2)) (not (= ?proj_s1 ?proj_s2))))
                           (iff
                            (incompat (eq_i ?i1 ?proj_i1) (eq_i ?i2 ?proj_i2))
                            (or (not (= ?i1 ?i2)) (not (= ?proj_i1 ?proj_i2)))))))
                 }
  :goal
  (forall (?pred_1 ?pred_2 ?ag1 ?ag2)
          (if
            (and
             (incompat ?pred_1 ?pred_2)
             (or
              (and (eval_pred ?pred_1 ?ag1) (eval_pred ?pred_2 ?ag2))
              (and (eval_pred ?pred_1 ?ag2) (eval_pred ?pred_2 ?ag1))))
            (not (= ?ag1 ?ag2))))
  }