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
public final class Common extends  BaseFormula{

    private final Value time;
    private final Formula formula;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Variable> boundVariables;
    private final Set<Value> allValues;

    private final int weight;



    public Common(Value time, Formula formula) {

        this.time = time;
        this.formula = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        this.variables = CollectionUtils.setFrom(formula.variablesPresent());
        this.boundVariables = CollectionUtils.setFrom(formula.getBoundVariables());
        this.allValues = Sets.newSet();
        this.allValues.add(time);

        this.subFormulae.add(this);

        if (time instanceof Variable) {
            variables.add((Variable) time);

        }

        this.weight =  time.getWeight() + formula.getWeight();
    }

    public Formula getFormula(){
        return formula;
    }

    public Value getTime() {
        return time;
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
    public Formula apply(Map<Variable, Value> substitution) {
        return   new Common(time.apply(substitution), formula.apply(substitution));

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
    public String toString() {
        return "(Common! "
                + time + " "+
                formula + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Common common = (Common) o;

        if (!time.equals(common.time)) return false;
        return formula.equals(common.formula);

    }

    @Override
    public int hashCode() {
        int result = time.hashCode();
        result = 31 * result + formula.hashCode();
        return result;
    }

    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return "Common";
    }

    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new Common(time, formula.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public Set<Variable> getBoundVariables() {
        return boundVariables;
    }


}
