{
 :name "Referential opacity should be satisfied"
 :sorts
 {
  A [],
  Agent [],
  Star []
  Number []
  }
 :signature
 {
  morning_star [Star]
  evening_star [Star]
  a [Agent]
  father [(Agent) Agent]
  age [(Agent) Number]
  P Boolean

 }
 :assumptions
 {
  "1" (not (Knows! a now (= morning_star evening_star)))
  "2" (= morning_star evening_star)
  "3" (Knows! a now (= morning_star morning_star))
  }
 :goal (and P (not P))
}
