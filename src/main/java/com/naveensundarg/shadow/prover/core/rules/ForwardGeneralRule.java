package com.naveensundarg.shadow.prover.core.rules;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 12/27/16.
 */
public interface ForwardGeneralRule<T> {

    Set<Formula> apply(Set<Formula> active, List<Formula> inputs);

    default Set<Formula> apply(Set<Formula> active, List<Formula> inputs, List<T> parameters) {

        return apply(active, inputs);
    }

}
