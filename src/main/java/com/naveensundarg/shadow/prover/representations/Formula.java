package com.naveensundarg.shadow.prover.representations;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/8/16.
 */
public abstract class Formula {

    public abstract Set<Formula> subFormulae();
    public abstract Set<Variable> variablesPresent();

    public abstract Formula apply(Map<Variable, Value> substitution);


    public abstract Formula applyOperation(UnaryOperator<Formula> operator);
}
