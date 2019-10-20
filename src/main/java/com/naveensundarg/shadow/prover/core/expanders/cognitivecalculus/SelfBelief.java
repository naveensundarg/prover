package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.representations.formula.Belief;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.utils.Constants;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.Set;
import java.util.stream.Collectors;

public enum SelfBelief implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Belief).
                filter(f -> ((Belief) f).getAgent().equals(Reader.I)).
                map(f -> ((Belief) f).getFormula()).
                collect(Collectors.toSet());

        if (!base.containsAll(derived)) {
            prover.getLogger().expansionLog(String.format("Belief(I, P) ==> P", Constants.VDASH, Constants.PHI, Constants.NEC, Constants.PHI), derived);

        }

        base.addAll(derived);
        added.addAll(derived);

    }
}
