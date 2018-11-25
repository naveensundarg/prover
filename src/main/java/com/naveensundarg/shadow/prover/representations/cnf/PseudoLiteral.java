package com.naveensundarg.shadow.prover.representations.cnf;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Not;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class PseudoLiteral {

    private final boolean isNegated;
    private final Formula predicate;
    private final int     weight;

    public PseudoLiteral(Formula atom, boolean isNegated) {

        this.predicate = atom;
        this.isNegated = isNegated;
        this.weight = atom.getWeight();
    }

    public Formula getFormula() {
        return predicate;
    }

    public boolean isNegated() {
        return isNegated;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        PseudoLiteral literal = (PseudoLiteral) o;

        if (isNegated != literal.isNegated) return false;
        return predicate.equals(literal.predicate);

    }

    public PseudoLiteral replace(Value x, Value y) {

        return new PseudoLiteral(predicate.replace(x, y), isNegated);
    }

    @Override
    public int hashCode() {
        int result = (isNegated ? 1 : 0);
        result = 31 * result + predicate.hashCode();
        return result;
    }

    public PseudoLiteral apply(Map<Variable, Value> subsitution) {
        return new PseudoLiteral((Predicate) predicate.apply(subsitution), isNegated);
    }

    @Override
    public String toString() {
        return isNegated ? "\u00AC" + predicate + "" : predicate + "";
    }

    public int getWeight() {
        return weight;
    }

    public static PseudoLiteral from(Formula formula) {

        if (formula instanceof Not) {
            return new PseudoLiteral(((Not) formula).getArgument(), true);
        } else {
            return new PseudoLiteral(formula, false);
        }
    }

    public Set<Variable> variables() {
        return predicate.variablesPresent();
    }
}
