package com.naveensundarg.shadow.prover.representations.cnf;

import com.naveensundarg.shadow.prover.representations.Atom;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.Vector;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Clause {

    private final Set<Literal> literals;

    public Clause(Atom atom){
        this.literals = Sets.with(new Literal(atom, false));
    }

    public static Clause fromClauses(List<Clause> clauses){

        return new Clause(clauses.stream().map(Clause::getLiterals).reduce(Sets.newSet(), Sets::union));
    }

    public Clause(Set<Literal> literals){
        this.literals  = Collections.unmodifiableSet(literals);
    }

    public Set<Literal> getLiterals() {
        return literals;
    }


    @Override
    public String toString() {
        return literals.stream().map(Literal::toString).reduce("", (x,y)-> x.isEmpty()? y: x+ " \u2228 " + y);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Clause clause = (Clause) o;

        return literals.equals(clause.literals);

    }

    @Override
    public int hashCode() {
        return literals.hashCode();
    }
}
