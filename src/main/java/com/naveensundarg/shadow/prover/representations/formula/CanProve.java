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
 * Created by naveensundarg on 5/4/16.
 */
public final class CanProve extends  BaseFormula{

    private final Formula formula;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;
    private final Set<Value> allValues;
    private final Set<Variable> boundVariables;
    private final int weight;

    public CanProve(Formula formula) {

        this.formula = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        this.subFormulae.add(this);

        this.allValues = Sets.newSet();

        this.variables = CollectionUtils.setFrom(formula.variablesPresent());
        this.values = CollectionUtils.setFrom(formula.valuesPresent());
        this.boundVariables = CollectionUtils.setFrom(formula.boundVariablesPresent());

        this.weight = 1 + formula.getWeight();
    }

    public Formula getFormula(){
        return formula;
    }



    public Set<Formula> getSubFormulae() {
        return subFormulae;
    }

    public Set<Variable> getVariables() {
        return variables;
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
    public Set<Value> valuesPresent() {
        return values;
    }

    @Override
    public Formula apply(Map<Variable, Value> substitution) {
        return new CanProve(formula.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        return new Atom("|"+ CommonUtils.sanitizeShadowedString(toString())+"|");
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return null;
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
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new CanProve(formula.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CanProve canProve = (CanProve) o;

        return formula.equals(canProve.formula);
    }

    @Override
    public int hashCode() {
        return safeHashCode(formula);
    }


    @Override
    public String toString() {
        return "(CanProve " +
                  formula +
                ')';
    }

    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return "CanProve";
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }
}
