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
public class Communicates extends BaseFormula{

    private final Value agent1;
    private final Value agent2;
    private final Value time;
    private final Formula formula;
    private final Set<Formula> subFormulae;

    private Set<Variable> variables;
    private Set<Value> values;

    private final Set<Variable> boundVariables;

    private final Set<Value> allValues;

    private final int weight;

    public Communicates(Value agent1,Value agent2, Value time, Formula formula) {


        this.agent1 = agent1;
        this.agent2 = agent2;
        this.time = time;
        this.formula = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        this.subFormulae.add(this);

        this.allValues = Sets.newSet();
        this.allValues.add(agent1);
        this.allValues.add(agent2);

        this.allValues.add(time);

        this.variables = CollectionUtils.setFrom(formula.variablesPresent());
        this.values = CollectionUtils.setFrom(formula.valuesPresent());

        this.boundVariables = CollectionUtils.setFrom(formula.boundVariablesPresent());

        if (agent1 instanceof Variable) {
            variables.add((Variable) agent1);
        }

        if (agent2 instanceof Variable) {
            variables.add((Variable) agent2);
        }
        if (time instanceof Variable) {
            variables.add((Variable) time);

        }

        this.weight = 1 + agent1.getWeight()  + agent2.getWeight() + time.getWeight()  + formula.getWeight();
    }

    public Formula getFormula(){
        return formula;
    }


    public Value getOrigin() {
        return agent1;
    }

    public Value getDestination() {
        return agent1;
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


    @Override
    public String toString() {
        return "(Communicates! "
                + agent1 + " "
                + agent2 + " "

                + time + " "+
                formula + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Communicates that = (Communicates) o;

        if (!agent1.equals(that.agent1)) return false;
        if (!agent2.equals(that.agent2)) return false;
        if (!time.equals(that.time)) return false;
        return formula.equals(that.formula);
    }

    @Override
    public int hashCode() {
        int result = safeHashCode(agent1);
        result = 31 * result + safeHashCode(agent2);
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
        return "Communicates";
    }


    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new Communicates(agent1, agent2, time, formula.replaceSubFormula(oldFormula, newFormula));
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
