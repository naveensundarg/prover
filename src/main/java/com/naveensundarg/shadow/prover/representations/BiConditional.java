package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Set;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class BiConditional extends Formula {
    private final Formula left;
    private final Formula right;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;

    public BiConditional(Formula left, Formula right){

        this.left = left;
        this.right = right;

        this.subFormulae = Sets.union(left.subFormulae(), right.subFormulae());

        this.variables = Sets.union(left.variablesPresent(), right.variablesPresent());
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
}
