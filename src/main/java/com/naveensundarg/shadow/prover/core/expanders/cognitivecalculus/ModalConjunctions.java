package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.And;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Constants;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public enum ModalConjunctions implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<And> level2Ands = CommonUtils.level2FormulaeOfType(base, And.class);

        for (And and : level2Ands) {

            Justification j = InferenceJustification.from(this.getClass().getSimpleName(), and);

            Set<Formula> level2Conjuncts = Arrays.stream(and.getArguments()).
                    filter(conjunct -> conjunct.getLevel() == 2).
                    filter(x -> !added.contains(x)).
                    map(x-> x.setJustification(j)).
                    collect(Collectors.toSet());

            added.addAll(level2Conjuncts);
            base.addAll(level2Conjuncts);


        }
    }

}
