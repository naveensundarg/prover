package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
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

            base.add(new Implication(biConditional.getLeft(), biConditional.getRight()));
            base.add(new Implication(biConditional.getRight(), biConditional.getLeft()));
            base.add(new Implication(Logic.negated(biConditional.getLeft()), Logic.negated(biConditional.getRight())));
            base.add(new Implication(Logic.negated(biConditional.getRight()), Logic.negated(biConditional.getLeft())));

        });

    }


}
