package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Sets;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 5/4/16.
 */
public class Trait extends  BaseFormula{

    private final List<Variable> traitVariables;
    private final Value agent;
    private final Value time;
    private final Value actionType;
    private final Formula triggeringCondition;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;

    private final Set<Value> allValues;

    private final int weight;

    public Trait(List<Variable>  variables, Value agent, Value time, Formula triggeringCondition, Value actionType) {

        this.traitVariables = variables;

        this.agent = agent;
        this.time = time;
        this.triggeringCondition = triggeringCondition;
        this.actionType = actionType;

        this.subFormulae = CollectionUtils.setFrom(triggeringCondition.subFormulae());
        this.subFormulae.add(this);

        this.allValues = Sets.newSet();
        this.allValues.add(agent);
        this.allValues.add(time);
        this.allValues.addAll(actionType.subValues());

        this.variables = Sets.union(agent.variablesPresent(), Sets.union(time.variablesPresent(), CollectionUtils.setFrom(triggeringCondition.variablesPresent())));
        this.variables.addAll(traitVariables);
        this.values = Sets.union(agent.subValues(), Sets.union(time.subValues(), CollectionUtils.setFrom(triggeringCondition.valuesPresent())));


        this.weight = 1 + agent.getWeight() + time.getWeight()  + triggeringCondition.getWeight();
    }

    public Formula getTriggeringCondition(){
        return triggeringCondition;
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
    public Formula apply(Map<Variable, Value> substitution) {
        return null;
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

    public Value getActionType() {
        return actionType;
    }

    public List<Variable> getTraitVariables() {
        return traitVariables;
    }

    @Override
    public String toString() {
        return "(Trait! " + "(" + StringUtils.trim((traitVariables).stream().map(Variable::toString).reduce("", (x, y) -> x  + y + " "))
                + ") "
                + agent + " "
                + time + " "+
                triggeringCondition + " "+
                actionType + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Trait says = (Trait) o;

        if (!agent.equals(says.agent)) return false;
        if (!time.equals(says.time)) return false;
        return triggeringCondition.equals(says.triggeringCondition);

    }

    @Override
    public int hashCode() {
        int result = safeHashCode(agent);
        result = 31 * result + safeHashCode(time);
        result = 31 * result + safeHashCode(triggeringCondition);
        return result;
    }

    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return "Says";
    }


    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new Trait(traitVariables, agent, time, triggeringCondition.replaceSubFormula(oldFormula, newFormula), actionType);
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return triggeringCondition.boundVariablesPresent();
    }

    @Override
    public Set<Value> valuesPresent() {
        return values;
    }
}
