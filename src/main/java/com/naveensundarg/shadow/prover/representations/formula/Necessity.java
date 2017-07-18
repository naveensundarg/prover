package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 7/9/16.
 */
public class Necessity extends  BaseFormula{

    private final Formula formula;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Variable> boundVariables;

    private final Set<Value> allValues;

    private final int weight;

    public Necessity(Formula formula) {


        this.formula = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        this.subFormulae.add(this);
        this.variables = CollectionUtils.setFrom(formula.variablesPresent());
        this.boundVariables = CollectionUtils.setFrom(formula.boundVariablesPresent());

        this.allValues = Sets.newSet();



        this.weight = 1 + formula.getWeight();
    }


    public Formula getFormula(){
        return formula;
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
        return new Necessity(formula.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        return new Atom("|"+ CommonUtils.sanitizeShadowedString(toString())+"|");

    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new Necessity(formula.applyOperation(operator));

    }

    @Override
    public int getLevel() {
        return 2;
    }

    @Override
    public int getWeight() {
        return weight;
    }


    @Override
    public String toString() {
        return "(nec "
           +
                formula + ")";
    }


    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return "nec";
    }

    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new Necessity(formula.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }

    @Override
    public Set<Value> valuesPresent() {
        return formula.valuesPresent();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Necessity necessity = (Necessity) o;

        return formula.equals(necessity.formula);
    }

    @Override
    public int hashCode() {
        return safeHashCode(formula);
    }
}
