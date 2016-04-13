package com.naveensundarg.shadow.prover.representations.cnf;

import com.naveensundarg.shadow.prover.representations.Predicate;
import com.naveensundarg.shadow.prover.representations.Value;
import com.naveensundarg.shadow.prover.representations.Variable;

import java.util.Map;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Literal {

    private final boolean isNegated;
    private final Predicate predicate;

    public Literal(Predicate atom, boolean isNegated){

        this.predicate = atom;
        this.isNegated = isNegated;
    }

    public Predicate getPredicate() {
        return predicate;
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
        return predicate.equals(literal.predicate);

    }

    @Override
    public int hashCode() {
        int result = (isNegated ? 1 : 0);
        result = 31 * result + predicate.hashCode();
        return result;
    }

    public Literal apply(Map<Variable, Value> subsitution){
        return new Literal((Predicate)predicate.apply(subsitution), isNegated);
    }
    @Override
    public String toString() {
        return  isNegated? "\u00AC"+ predicate +"": predicate +"";
    }
}
