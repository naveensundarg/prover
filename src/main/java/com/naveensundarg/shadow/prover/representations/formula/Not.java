package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Not extends Formula {

    private final Formula argument;

    private final Set<Formula> subFormulae;


    private final int weight;
    public Not(Formula argument){
        this.argument = argument;
        this.subFormulae = CollectionUtils.setFrom(argument.subFormulae());
        this.subFormulae.add(this);
        this.weight = 1 + argument.getWeight();
    }



    public Formula getArgument() {
        return argument;
    }



    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Not not = (Not) o;

        return argument.equals(not.argument);

    }

    @Override
    public int hashCode() {
        return safeHashCode(argument);
    }

    @Override
    public String toString() {
        return "(not " + argument +")";
    }

    @Override
    public Set<Formula> subFormulae() {
        return subFormulae;
    }

    @Override
    public Set<Variable> variablesPresent(){
        return argument.variablesPresent();
    }

    @Override
    public Formula apply(Map<Variable, Value> substitution) {
        return new Not(argument.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        return new Not(argument.shadow(level));
    }
    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new Not(argument.applyOperation(operator));
    }

    @Override
    public int getLevel() {
        return argument.getLevel();
    }

    @Override
    public int getWeight() {
        return weight;
    }

    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new Not(argument.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return argument.boundVariablesPresent();
    }

    @Override
    public Set<Value> valuesPresent() {
        return argument.valuesPresent();
    }
}
