package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class BiConditional extends Formula {
    private final Formula left;
    private final Formula right;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;

    private final int weight;
    public BiConditional(Formula left, Formula right){

        this.left = left;
        this.right = right;

        this.subFormulae = Sets.union(left.subFormulae(), right.subFormulae());

        this.variables = Sets.union(left.variablesPresent(), right.variablesPresent());

        this.weight = 1 + 2 * (1 + left.getWeight() + right.getWeight());
    }

    public Formula getLeft() {
        return left;
    }

    public Formula getRight() {
        return right;
    }


    @Override
    public String toString() {
        return "(iff " + left + " " + right + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BiConditional that = (BiConditional) o;

        if (!left.equals(that.left)) return false;
        return right.equals(that.right);

    }

    @Override
    public int hashCode() {
        int result = left.hashCode();
        result = 31 * result + right.hashCode();
        return result;
    }

    @Override
    public Set<Formula> subFormulae() {
        return subFormulae;
    }

    @Override
    public Set<Variable> variablesPresent() {
        return variables;
    }

    @Override
    public Formula apply(Map<Variable, Value> substitution) {
        return new BiConditional(left.apply(substitution), right.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        return new BiConditional(left.shadow(level), right.shadow(level));
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new BiConditional(left.applyOperation(operator), right.applyOperation(operator));
    }

    @Override
    public int getLevel() {
        return Math.max(left.getLevel(), right.getLevel());
    }

    @Override
    public int getWeight() {
        return weight;
    }
}
