package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;

import java.util.Set;

public enum SaysR1 implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<Says> says = CommonUtils.formulaOfType(base, Says.class);

        says.forEach(say -> {

            Justification j = InferenceJustification.from(this.getClass().getSimpleName(), say);
            Formula f = say.getFormula();
            Value agent = say.getAgent();

            Formula f1 = new Implication(
                    new And(say, new Predicate(Logic.TRUTHFUL, new Value[]{agent})),
                    f
            ).setJustification(j);

            Formula f2 = new Implication(
                    new And(say, new Not(new Predicate(Logic.TRUTHFUL, new Value[]{agent}))),
                    Logic.negated(f)
            ).setJustification(j);
            base.add(f1);
            base.add(f2);

        });

    }


}
