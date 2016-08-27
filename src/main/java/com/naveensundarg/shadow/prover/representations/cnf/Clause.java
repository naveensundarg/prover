package com.naveensundarg.shadow.prover.representations.cnf;

import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.binaryProduct;
import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Clause {

    private final Set<Literal> literals;

    public Clause(Predicate P){
        this.literals = Sets.with(new Literal(P, false));
    }

    public static Clause fromClauses(List<Clause> clauses){

        return new Clause(clauses.stream().map(Clause::getLiterals).reduce(newSet(), Sets::union));
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

    public Clause replace(Value value1, Value value2){

        return new Clause(literals.stream().map(literal -> literal.replace(value1, value2)).collect(Collectors.toSet()));
    }



    public Clause apply(Map<Variable, Value> substitution){

        return new Clause(literals.stream().map(l->l.apply(substitution)).collect(Collectors.toSet()));
    }
    @Override
    public int hashCode() {
        return literals.hashCode();
    }


    public Clause refactor(){

        Set<Literal> tempLiterals = newSet();
        tempLiterals.addAll(literals);
        boolean changed = true;
        while(changed){

            Set<List<Literal>> literalPairs = binaryProduct(tempLiterals);
            changed = false;
            for(List<Literal> literalPair: literalPairs){

                Literal first = literalPair.get(0);
                Literal second = literalPair.get(1);

                if(!first.equals(second)){

                   Map<Variable, Value> theta = first.unify(second);

                    if(theta!=null){

                       // tempLiterals.remove(first);
                      //  tempLiterals.remove(second);
                     //   tempLiterals.add(first.apply(theta));

                        tempLiterals = tempLiterals.stream().map(x->x.apply(theta)).collect(Collectors.toSet());
                        changed = true;
                    }
                }



            }

        }
        return new Clause(tempLiterals);
    }
}
