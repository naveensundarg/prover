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
public class Desire extends BaseFormula{

    private final Value agent;
    private final Value time;
    private final Formula formula;
    private final Set<Formula> subFormulae;

    private final Set<Variable> variables;
    private final Set<Value> values;

    private final Set<Variable> boundVariables;

    private final Set<Value> allValues;

    private final int weight;

    public Desire(Value agent, Value time, Formula formula) {


        this.agent = agent;
        this.time = time;
        this.formula = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        subFormulae.add(this);

        this.allValues = Sets.newSet();
        this.allValues.add(agent);
        this.allValues.add(time);

        this.variables = CollectionUtils.setFrom(formula.variablesPresent());
        this.values = Sets.union(Sets.union(agent.subValues(), time.subValues()), formula.valuesPresent());

        this.boundVariables = CollectionUtils.setFrom(formula.boundVariablesPresent());

        if (agent instanceof Variable) {
            variables.add((Variable) agent);
        }

        if (time instanceof Variable) {
            variables.add((Variable) time);

        }

        this.weight = 1 + agent.getWeight() + time.getWeight() + formula.getWeight();
    }

    public Value getAgent() {
        return agent;
    }

    public Value getTime() {
        return time;
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
        return new Desire(agent.apply(substitution), time.apply(substitution), formula.apply(substitution));
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
        return "(Desires! "
                + agent + " "
                + time + " "+
                formula + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Desire knowledge = (Desire) o;

        if (!agent.equals(knowledge.agent)) return false;
        if (!time.equals(knowledge.time)) return false;
        return formula.equals(knowledge.formula);

    }

    @Override
    public int hashCode() {
        int result = safeHashCode(agent);
        result = 31 * result + safeHashCode(time);
        result = 31 * result + safeHashCode(formula);
        return result;
    }

    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return "Desires";
    }


    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new Desire(agent, time, formula.replaceSubFormula(oldFormula, newFormula));
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
