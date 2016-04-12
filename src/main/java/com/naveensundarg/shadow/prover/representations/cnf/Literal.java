package com.naveensundarg.shadow.prover.representations.cnf;

import com.naveensundarg.shadow.prover.representations.Atom;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Literal {

    private final boolean isNegated;
    private final Atom atom;

    public Literal(Atom atom, boolean isNegated){

        this.atom = atom;
        this.isNegated = isNegated;
    }

    public Atom getAtom() {
        return atom;
    }

    public boolean isNegated() {
        return isNegated;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Literal literal = (Literal) o;

        if (isNegated != literal.isNegated) return false;
        return atom.equals(literal.atom);

    }

    @Override
    public int hashCode() {
        int result = (isNegated ? 1 : 0);
        result = 31 * result + atom.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return  isNegated? "\u00AC"+ atom +"": atom+"";
    }
}
