

{:assumptions
  {A1 (Perceives! student t1 (Believes! (embodiment a) t1  (Holds (Prop watch stopped) t1 )))
   A2 (Perceives! student t2 (Believes! (embodiment b) t2  (Holds (Prop watch stopped) t2)))


   A3 (Believes! student t3  (PersonalObject watch))
   A4 (Believes! student t4  (if (exists [?agent1 ?agent2 ?u ?prop ?time1 ?time2]  (and  (PersonalObject ?u)
                                               (Believes! ?agent1 ?time1  (Holds (Prop ?u ?prop) ?time1))
                                               (Believes! ?agent2 ?time2  (Holds (Prop ?u ?prop) ?time2 ))))
                             (= (identityOf ?agent1) (identityOf ?agent2))))}


 :goal  (Believes! student t5 (= (identityOf (embodiment a)) (identityOf (embodiment b))))}


