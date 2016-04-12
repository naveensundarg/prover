package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class BiConditional extends Formula {
    private final Formula left;
    private final Formula right;
    private final Set<Formula> subFormulae;

    public BiConditional(Formula left, Formula right){

        this.left = left;
        this.right = right;

        this.subFormulae = Sets.union(left.subFormulae(), right.subFormulae());
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
}
