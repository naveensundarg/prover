package com.naveensundarg.shadow.prover.core.propositionalmodalprovers;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Sets;

public class LP2 extends LP {

    @Override
    public boolean canApplyRule(Formula f) {
        return Logic.isConsistent(Sets.with(f));
    }

}
