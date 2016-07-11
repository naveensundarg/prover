package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Universal extends Formula implements Quantifier {

    private final Formula argument;
    private final Variable[] vars;
    private Set<Formula> subFormulae;
    private final Set<Variable> variables;


    public Universal(Variable[] vars, Formula argument) {

        if (!(vars.length > 0)) {
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
        //TODO:
        return new Universal(vars, argument.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        if (level == 0) {

            return new Atom("#"+this.toString()+"#");

        } else if (level == 1) {

            return new Universal(vars, argument.shadow(level));
        }

        throw new AssertionError("Invalid shadow getLevel: " + level);
    }


    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new Universal(vars, argument.applyOperation(operator));
    }

    @Override
    public int getLevel() {
        return 1;
    }

    public Variable[] vars() {
        return vars;
    }

    @Override
    public String toString() {
        return "(\u2200 " + Arrays.toString(vars) + " "
                + argument.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Universal universal = (Universal) o;

        if (argument != null ? !argument.equals(universal.argument) : universal.argument != null) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(vars, universal.vars)) return false;
        return subFormulae != null ? subFormulae.equals(universal.subFormulae) : universal.subFormulae == null;

    }

    @Override
    public int hashCode() {
        int result = argument != null ? argument.hashCode() : 0;
        result = 31 * result + Arrays.hashCode(vars);
        result = 31 * result + (subFormulae != null ? subFormulae.hashCode() : 0);
        return result;
    }
}
