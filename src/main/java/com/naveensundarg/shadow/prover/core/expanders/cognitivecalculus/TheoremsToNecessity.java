package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Knowledge;
import com.naveensundarg.shadow.prover.representations.formula.Necessity;
import com.naveensundarg.shadow.prover.utils.Constants;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;
import java.util.stream.Collectors;

public enum TheoremsToNecessity implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        Set<Formula> theorems = base.stream().map(Formula::subFormulae).reduce(Sets.newSet(), Sets::union).
                stream().filter(x -> !x.equals(goal) && prover.prove(Sets.newSet(), x).isPresent()).collect(Collectors.toSet());

        Set<Formula> necs = theorems.stream().map(Necessity::new).collect(Collectors.toSet());

        prover.getLogger().expansionLog(String.format("{} %s %s ==>  %s %s", Constants.VDASH, Constants.PHI, Constants.NEC, Constants.PHI), necs);

        base.addAll(necs);
        added.addAll(necs);
    }


}
