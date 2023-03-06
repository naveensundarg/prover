package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Constants;

import java.util.Set;
import java.util.stream.Collectors;

public enum NotExistsToForallNot implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Not).
                map(f -> (Not) f).
                filter(not -> not.getArgument() instanceof Existential).
                map(notExists -> {
                    Existential existential = (Existential) notExists.getArgument();
                    Variable[]  variables   = existential.vars();
                    Formula     kernel      = existential.getArgument();
                    Formula newFormula =   new Universal(variables, new Not(kernel));
                    newFormula.setJustificationLabelAndAncestors(this.getClass().getSimpleName(), CollectionUtils.listOf(notExists));
                    return newFormula;
                }).collect(Collectors.toSet());

        prover.getLogger().expansionLog("Not exists => Forall not", derived);


        base.addAll(derived);
        added.addAll(derived);

    }


}
