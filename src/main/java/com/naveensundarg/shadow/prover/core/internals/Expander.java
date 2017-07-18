package com.naveensundarg.shadow.prover.core.internals;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Set;

/**
 * Created by naveensundarg on 7/17/17.
 */

@FunctionalInterface
public interface Expander {


    void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal);

}
