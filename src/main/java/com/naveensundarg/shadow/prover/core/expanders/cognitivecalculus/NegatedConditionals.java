package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;

import java.util.Set;
import java.util.stream.Collectors;

public enum NegatedConditionals implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {



        Set<Not> allNots = CommonUtils.formulaOfType(base, Not.class);
        Set<Not> negatedConditional = allNots.stream().filter(x->x.getArgument() instanceof Implication).collect(Collectors.toSet());


        negatedConditional.forEach(negatedCondition -> {

            Justification j = InferenceJustification.from(this.getClass().getSimpleName(), negatedCondition);
            Implication implication = (Implication) negatedCondition.getArgument();
            base.add(new And(
                    implication.getAntecedent(),
                    Logic.negated(implication.getConsequent())
            ).setJustification(j));

        });

    }


}
