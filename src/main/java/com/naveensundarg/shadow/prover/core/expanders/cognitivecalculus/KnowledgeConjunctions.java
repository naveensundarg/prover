package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.And;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.Constants;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public enum KnowledgeConjunctions implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Knowledge).
                map(f -> (Knowledge) f).
                filter(k -> k.getFormula() instanceof And).
                flatMap(k -> {

                    Value agent = k.getAgent();
                    Value time  = k.getTime();
                    And   and   = (And) k.getFormula();

                    Justification j = InferenceJustification.from(this.getClass().getSimpleName(), k);

                    return Arrays.stream(and.getArguments()).map(conjunct -> new Knowledge(agent, time, conjunct).setJustification(j));

                }).collect(Collectors.toSet());


        prover.getLogger().expansionLog("Know(P and Q) ==> Know(P) and Know(Q)", derived);

        base.addAll(derived);
        added.addAll(derived);

    }


}
