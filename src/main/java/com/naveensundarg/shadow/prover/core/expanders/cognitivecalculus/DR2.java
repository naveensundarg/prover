package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Common;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Constants;

import java.util.Set;
import java.util.stream.Collectors;

public enum DR2 implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        Set<Common> commons = CommonUtils.level2FormulaeOfType(base, Common.class);
        Set<Value>  agents  = Logic.allAgents(CollectionUtils.addToSet(base, goal));

        for (Common c : commons) {
            for (Value agent : agents) {
                Formula   formula   = c.getFormula();
                Value     time      = c.getTime();
                Knowledge knowledge = new Knowledge(agent, time, formula);


                Justification j = InferenceJustification.from(this.getClass().getSimpleName(), c);

                if (!added.contains(knowledge)) {
                    base.add(knowledge.setJustification(j));
                    added.add(knowledge);
                }
            }
        }
        ;

    }


}
