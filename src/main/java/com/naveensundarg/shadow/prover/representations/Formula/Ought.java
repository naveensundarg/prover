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
public class Ought extends Formula implements BaseFormula{


    Value agent;
    Value time;
    Formula precondition;
    Formula ought;

    Set<Formula> subFormulae;
    Set<Variable> variables;
    private final Set<Value> allValues;


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

        this.variables = CollectionUtils.setFrom(formula.variablesPresent());
        if (agent instanceof Variable) {
            variables.add((Variable) agent);
        }

        if (time instanceof Variable) {
            variables.add((Variable) time);

        }
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
        int result = agent.hashCode();
        result = 31 * result + time.hashCode();
        result = 31 * result + precondition.hashCode();
        result = 31 * result + ought.hashCode();
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
