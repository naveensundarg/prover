package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Intends;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.formula.Perception;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Constants;

import java.util.Set;
import java.util.stream.Collectors;

public enum IntentionToPerception implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Intends).
                map(f -> {
                    Intends    i = (Intends) f;
                    Perception k = new Perception(i.getAgent(), i.getTime(), i.getFormula());
                    k.setJustification(new CompoundJustification("Intention to Perception " + i, CollectionUtils.listOf(i.getJustification())));

                    Justification j = InferenceJustification.from(this.getClass().getSimpleName(), i);

                    return k.setJustification(j);
                }).
                collect(Collectors.toSet());

        prover.getLogger().expansionLog(String.format("Intends(P) ==> Perceives(P)", Constants.VDASH, Constants.PHI, Constants.NEC, Constants.PHI), derived);

        base.addAll(derived);
        added.addAll(derived);

    }
}
