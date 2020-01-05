package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Variable;
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

            base.add(new Implication(biConditional.getLeft(), biConditional.getRight()).setJustification(j));
            base.add(new Implication(biConditional.getRight(), biConditional.getLeft()).setJustification(j));
            base.add(new Implication(Logic.negated(biConditional.getLeft()), Logic.negated(biConditional.getRight())).setJustification(j));
            base.add(new Implication(Logic.negated(biConditional.getRight()), Logic.negated(biConditional.getLeft())).setJustification(j));

        });

    }


}
