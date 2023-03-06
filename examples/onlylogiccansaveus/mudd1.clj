{:name "I, Mudd (1)"
 :skip false
 :description
 "The inconsistency is of the cognitive variety & is that … to Norman it is Communicated that
  the crew members desire to desire that an event they don’t desire to happen happens."
 :assumptions
 {;; joy is the opposite of sorrow
   :background-1  (= sorrow (opposite joy))
   :background-1a (forall [x y]
                          (if  (= (opposite x) y)
                            (= y (opposite x))))
   :background-1b (forall [x] (if (Desires! crew x) (not (Desires! crew (opposite x)))))
   ;; the
   :A1            (Believes! norman (Desires! crew joy))
   :A2            (Says! crew (Desires! crew sorrow))}
 :goal (Believes! norman False)}


{:name "I, Mudd (2)"
 :skip false
 :description
 "Norman perceives that crew members perceive there is an explosion.
  If P to  K, then we have a contradiction, because P P E (explosion), so K E; but Norman perceives ~E,
  so K ~E; hence E & ~E in Norman’s mind.  Yet he survives."
 :assumptions
 {:A1 (Perceives! norman (not (happens explosion now)))
  :A2 (Perceives! norman (Perceives! crew (happens explosion now)))}
 :goal (Believes! norman False)}


{:name "I, Mudd (3)"
 :skip false
 :description
 "Let L be the sentence “This sentence is false.”
  (Harry tells Norman “I’m lying,” but this is simply short for “This sentence is false,” the sentence we’ve labeled L.)
  Comm(harry, L).
  That which is communicated by an agent is either true or false (when coherent and when the context is classical, object-level, and bivalent, which we assume here).  Suppose in the first case that what Harry has communicated is true; then L is true.
  But since what L itself says is that it’s false, it follows under our supposition that L is true.
  That is, if L is false then L is true.  Very well, now for the second case:  suppose on the other hand that Harry’s communication is false.
  Since L is Harry’s communication. what L itself says is false.
  But L says that L is false, which entails that L is under this supposition true.
  Hence, if L is false then L is true.
  Combing the two conditionals we have now proved yields that L is true if and only if L is false — which leads to an outright contradiction:
  either L is true and L is false, or vice versa.
  Norman believes falsum & his mind/brain explodes — perhaps because he now believes everything …  QED"
 :assumptions
 {:A1 (Says! harry (not (TRUTHFUL harry)))}

 :goal (Believes! norman False)}


