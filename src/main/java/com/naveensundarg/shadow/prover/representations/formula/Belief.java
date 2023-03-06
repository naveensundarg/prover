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
public final class Belief extends BaseFormula implements UnaryModalFormula{

    private final Value agent;
    private final Value time;
    private final Formula formula;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;

    private final Set<Variable> boundVariables;
    private final Set<Value> allValues;

    private final int weight;


    public Belief(Value agent, Value time, Formula formula) {


        this.agent = agent;
        this.time = time;
        this.formula = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        this.subFormulae.add(this);
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
    public Belief(Value agent, Value time, Formula formula, int strengthFactor) {
        this(agent, time, formula);
        this.setStrengthFactor(strengthFactor);
    }

    public Formula getFormula(){
        return formula;
    }


    public Value getAgent() {
        return agent;
    }

    public Value getTime() {
        return time;
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
        return new Belief(agent.apply(substitution), time.apply(substitution), formula.apply(substitution));

    }

    @Override
    public  Formula generalize(Map<Value, Variable> substitution){

        return new Belief(agent.generalize(substitution), time.generalize(substitution), formula.generalize(substitution));
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


        return new Belief(agent, time, formula.replaceSubFormula(oldFormula, newFormula));
    }


    @Override
    public String toString() {
        return "(Believes! "
                + agent + " "
                + time + " "+
                formula + ")";
    }

    @Override
    public String toSnarkString() {
        return "(Believes! "
                + agent.toSnarkString() + " "
                + time.toSnarkString() + " "+
                formula.toSnarkString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Belief belief = (Belief) o;

        if (!agent.equals(belief.agent)) return false;
        if (!time.equals(belief.time)) return false;
        return formula.equals(belief.formula);

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
        return "Belief of " +  ((formula instanceof Predicate)? ((Predicate) formula).getName():formula.getClass() +"");
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }
}
