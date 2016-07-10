package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Existential extends Formula implements Quantifier{

    private final Formula argument;
    private final Variable[] vars;
    private Set<Formula> subFormulae;
    private final Set<Variable> variables;


    public Existential(Variable[] vars, Formula argument){

        if(!(vars.length>0)){
            throw new AssertionError("Existential should have at least one variable");
        }

        this.vars = vars;
        this.argument = argument;
        this.subFormulae = Sets.with(argument);
        this.variables = argument.variablesPresent();

        Arrays.stream(vars).forEach(this.variables::add);
    }

    public Formula getArgument() {
        return argument;
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
        //TODO: Variable capture
        return new Existential(vars, argument.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        if (level == 0) {

            new Atom("#"+this.toString()+"#");

        } else if (level == 1) {

            return new Existential(vars, argument.shadow(level));
        }

        throw new AssertionError("Invalid shadow level: " + level);
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new Existential(vars, argument.applyOperation(operator));
    }

    public Variable[] vars() {
        return vars;
    }

    @Override
    public String toString() {
        return "(\u2203 " + Arrays.toString(vars) +" "
                + argument.toString() +")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Existential existential = (Existential) o;

        if (argument != null ? !argument.equals(existential.argument) : existential.argument != null) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(vars, existential.vars)) return false;
        return subFormulae != null ? subFormulae.equals(existential.subFormulae) : existential.subFormulae == null;

    }

    @Override
    public int hashCode() {
        int result = argument != null ? argument.hashCode() : 0;
        result = 31 * result + Arrays.hashCode(vars);
        result = 31 * result + (subFormulae != null ? subFormulae.hashCode() : 0);
        return result;
    }
}
