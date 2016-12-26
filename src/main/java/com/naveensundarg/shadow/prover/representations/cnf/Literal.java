package com.naveensundarg.shadow.prover.representations.cnf;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Literal {

    private final boolean isNegated;
    private final Predicate predicate;
    private final int weight;

    public Literal(Predicate atom, boolean isNegated){

        this.predicate = atom;
        this.isNegated = isNegated;
        this.weight = atom.getWeight();
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

    public  Literal replace(Value x, Value y){

        return new Literal(predicate.replace(x,y), isNegated);
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


    public Map<Variable, Value> unify(Literal other){

        if(!this.isNegated && !other.isNegated){

            Map<Variable, Value> theta = Unifier.unify(this.predicate, other.predicate);

            return theta;


        }

        return null;


    }

    public int getWeight() {
        return weight;
    }

    public Optional<Map<Variable, Value>> subsubmes(Literal other){


        if(this.isNegated() != other.isNegated()){

            return Optional.empty();
        }
        else {

            return this.getPredicate().subsumes(other.getPredicate());
        }

    }


    public Set<Variable> variables(){
        return predicate.variablesPresent();
    }
}
