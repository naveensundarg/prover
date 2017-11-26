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
 * Created by naveensundarg on 11/24/16.
 */
public class Ought extends BaseFormula{


    private final Value agent;
    private final Value time;
    private final Formula precondition;
    private final Formula ought;

    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;

    private final Set<Variable> boundVariables;

    private final Set<Value> allValues;

    private final int weight;

    public Ought(Value agent, Value time, Formula formula, Formula ought) {


        this.agent = agent;
        this.time = time;
        this.precondition = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        this.ought = ought;
        this.subFormulae.add(this);
        this.allValues = Sets.newSet();
        this.allValues.add(agent);
        this.allValues.add(time);

        this.variables  = Sets.union(agent.variablesPresent(), Sets.union(time.variablesPresent(), Sets.union(formula.variablesPresent(), ought.variablesPresent())));
        this.values  = Sets.union(agent.subValues(), Sets.union(time.subValues(), Sets.union(formula.valuesPresent(), ought.valuesPresent())));
        this.boundVariables =  Sets.union(formula.boundVariablesPresent(), ought.boundVariablesPresent());

        if (agent instanceof Variable) {
            variables.add((Variable) agent);
        }

        if (time instanceof Variable) {
            variables.add((Variable) time);

        }

        this.weight = 1 + agent.getWeight() + time.getWeight() + precondition.getWeight() + ought.getWeight();
    }

    public Formula getPrecondition(){
        return precondition;
    }


    public Formula getOught() {
        return ought;
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
        return new Ought(agent, time, precondition.apply(substitution), ought.apply(substitution));
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


        return new Ought(agent, time, precondition.replaceSubFormula(oldFormula, newFormula), ought.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }

    @Override
    public Set<Value> valuesPresent() {
        return values;
    }


    @Override
    public String toString() {
        return "(Ought! "
                + agent + " "
                + time + " "+
                precondition + " " +
                 ought + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Ought ought1 = (Ought) o;

        if (!agent.equals(ought1.agent)) return false;
        if (!time.equals(ought1.time)) return false;
        if (!precondition.equals(ought1.precondition)) return false;
        return ought.equals(ought1.ought);
    }

    @Override
    public int hashCode() {
        int result = safeHashCode(agent);
        result = 31 * result + safeHashCode(time);
        result = 31 * result + safeHashCode(precondition);
        result = 31 * result + safeHashCode(ought);
        return result;
    }

    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return "Ought";
    }
}
