package com.naveensundarg.shadow.prover.representations.cnf;

import com.naveensundarg.shadow.prover.representations.Atom;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Collections;
import java.util.Set;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class CNFFormula {

    private final Set<Clause> clauses;


    public CNFFormula(Atom atom){

        this.clauses = Sets.with(new Clause(atom));
    }

    public CNFFormula(Set<Clause> clauses){
        this.clauses  = Collections.unmodifiableSet(clauses);
    }

    public Set<Clause> getClauses() {
        return clauses;
    }


    @Override
    public String toString() {
        return clauses.stream().map(Clause::toString).reduce("", (x,y)-> x.isEmpty()? y: x + "\n" +y);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CNFFormula that = (CNFFormula) o;

        return clauses.equals(that.clauses);

    }

    @Override
    public int hashCode() {
        return clauses.hashCode();
    }
}
