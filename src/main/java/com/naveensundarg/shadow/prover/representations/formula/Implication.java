package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Implication extends Formula{

    private final Formula antecedent;
    private final Formula consequent;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;

    private final Set<Variable> boundVariables;
    private final int weight;

    public Implication(Formula antecedent, Formula consequent){

        this.antecedent = antecedent;
        this.consequent = consequent;

        this.subFormulae = Sets.union(antecedent.subFormulae(), consequent.subFormulae());
        this.subFormulae.add(this);
        this.variables  = Sets.union(antecedent.variablesPresent(), consequent.variablesPresent());
        this.values  = Sets.union(antecedent.valuesPresent(), consequent.valuesPresent());
        this.boundVariables = Sets.union(antecedent.boundVariablesPresent(), consequent.boundVariablesPresent());
        this.weight = 1 + antecedent.getWeight() + consequent.getWeight();
    }

    public Formula getAntecedent() {
        return antecedent;
    }

    public Formula getConsequent() {
        return consequent;
    }


    @Override
    public String toString() {
        return "(implies " + antecedent + " " + consequent + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Implication that = (Implication) o;

        if (!antecedent.equals(that.antecedent)) return false;
        return consequent.equals(that.consequent);

    }

    @Override
    public int hashCode() {
        int result = safeHashCode(antecedent);
        result = 31 * result + safeHashCode(consequent);
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
        return new Implication(antecedent.apply(substitution), consequent.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        return new Implication(antecedent.shadow(level), consequent.shadow(level));
    }
    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new Implication(antecedent.applyOperation(operator), consequent.applyOperation(operator));
    }

    @Override
    public int getLevel() {
        return Math.max(antecedent.getLevel(),consequent.getLevel());
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


        return new Implication(antecedent.replaceSubFormula(oldFormula, newFormula), consequent.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }

    @Override
    public Set<Value> valuesPresent() {
        return values;
    }
}
