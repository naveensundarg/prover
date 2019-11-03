package com.naveensundarg.shadow.prover.core.propositionalmodalprovers;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

public class LP2 extends LP {

    @Override
    public boolean canApplyRule(Set<Formula> background, Formula f) {
        return Logic.isConsistent(Sets.add(background, f));
    }

}
