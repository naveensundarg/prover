package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.representations.formula.Belief;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.formula.Perception;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Constants;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.Set;
import java.util.stream.Collectors;

public enum PerceptionToKnowledge implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Perception).
                map(f -> {
                    Perception p = (Perception) f;
                    Knowledge  k = new Knowledge(p.getAgent(), p.getTime(), p.getFormula());
                    k.setJustification(new CompoundJustification("Perception to knowledge " + p, CollectionUtils.listOf(p.getJustification())));
                    return k;
                }).
                collect(Collectors.toSet());

        prover.getLogger().expansionLog(String.format("Perceives(P) ==> P", Constants.VDASH, Constants.PHI, Constants.NEC, Constants.PHI), derived);

        base.addAll(derived);
        added.addAll(derived);

    }
}
