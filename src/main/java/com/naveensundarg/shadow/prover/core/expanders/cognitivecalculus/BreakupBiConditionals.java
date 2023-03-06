package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;

import java.util.Set;
import java.util.stream.Collectors;

public enum BreakupBiConditionals implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {



        Set<BiConditional> biConditionals = CommonUtils.formulaOfType(base, BiConditional.class);

        biConditionals.forEach(biConditional -> {

            Justification j = InferenceJustification.from(this.getClass().getSimpleName(), biConditional);

            Formula f1 = new Implication(biConditional.getLeft(), biConditional.getRight()).setJustification(j);
            Formula f2 = new Implication(biConditional.getRight(), biConditional.getLeft()).setJustification(j);
            Formula f3 = new Implication(Logic.negated(biConditional.getLeft()), Logic.negated(biConditional.getRight())).setJustification(j);
            Formula f4 = new Implication(Logic.negated(biConditional.getRight()), Logic.negated(biConditional.getLeft())).setJustification(j);

            CollectionUtils.listOf(f1,f2,f3,f4).forEach(f-> f.setJustificationLabelAndAncestors(this.getClass().getSimpleName(), CollectionUtils.listOf(biConditional)));

            base.add(f1);
            base.add(f2);
            base.add(f3);
            base.add(f4);

        });

    }


}
