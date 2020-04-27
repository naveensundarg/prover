package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.representations.formula.Belief;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.UnaryModalFormula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.HashSet;
import java.util.Set;

public enum  InnerModalForward implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        if(goal instanceof UnaryModalFormula) {
            UnaryModalFormula formula = (UnaryModalFormula) goal;
            Value agent         = formula.getAgent();
            Value    time          = formula.getTime();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> innerGivens = agentSnapShot.allBelievedByAgentTillTime(agent, time);

            if(prover instanceof CognitiveCalculusProver) {

                CognitiveCalculusProver cognitiveCalculusProver = (CognitiveCalculusProver) prover;
                Set<Formula> innerAdded = Sets.newSet();
                Set<Formula> innerGivensCopy = new HashSet<>(innerGivens);
                if(!innerGivens.isEmpty()){
                    cognitiveCalculusProver.expand(innerGivensCopy, innerAdded, goal);
                    Sets.difference(innerGivensCopy, innerGivens).forEach(addedFormula -> base.add(new Belief(agent, time, addedFormula)));

                }


            }
             else {
                throw new AssertionError("InnerModalForward should be used only in a cognitive calculus.");

            }

        }
     }
}
