package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Constants;

import java.util.Set;
import java.util.stream.Collectors;

public enum SaysToBelief implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Says).
                map(f -> {
                    Says   s = (Says) f;
                    Belief b = new Belief(s.getAgent(), s.getTime(), s.getFormula());
                    b.setJustification(new CompoundJustification("Says to belief", CollectionUtils.listOf(s.getJustification())));
                    return b;
                }).
                collect(Collectors.toSet());

        prover.getLogger().expansionLog(String.format("Says(P) ==> Belief(P)", Constants.VDASH, Constants.PHI, Constants.NEC, Constants.PHI), derived);

        base.addAll(derived);
        added.addAll(derived);

    }
}
