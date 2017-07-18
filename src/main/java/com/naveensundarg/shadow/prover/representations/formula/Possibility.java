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
public class Possibility extends  BaseFormula{

    Formula formula;
    Set<Formula> subFormulae;
    Set<Variable> variables;
    private final Set<Value> allValues;

    private final int weight;

    public Possibility(Formula formula) {


        this.formula = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        subFormulae.add(this);
        this.variables = CollectionUtils.setFrom(formula.variablesPresent());
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
        return new Possibility(formula.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        return new Atom("|"+ CommonUtils.sanitizeShadowedString(toString())+"|");

    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new Possibility(formula.applyOperation(operator));
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
        return "(pos "
           +
                formula + ")";
    }


    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return "pos";
    }

    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new Possibility(formula.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return formula.boundVariablesPresent();
    }

    @Override
    public Set<Value> valuesPresent() {
        return formula.valuesPresent();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Possibility that = (Possibility) o;

        return formula.equals(that.formula);
    }

    @Override
    public int hashCode() {
        return safeHashCode(formula);
    }
}
