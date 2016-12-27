package com.naveensundarg.shadow.prover.core.rules;

import com.naveensundarg.shadow.prover.representations.cnf.Clause;

import java.util.Set;

/**
 * Created by naveensundarg on 4/15/16.
 */


@FunctionalInterface
public interface ForwardClauseRule {

    Set<Clause> apply(Clause clause1, Clause clause2);

}
