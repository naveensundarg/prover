(and
  (Controls ?agent2 ?agent1)
  (Believes! ?agent1
             (Desires! ?agent2 (holds ?goal ?t)))
  (Believes! ?agent1
             (necessary ?action ?goal))
  (requests ?agent1 ?agent2 ?action))