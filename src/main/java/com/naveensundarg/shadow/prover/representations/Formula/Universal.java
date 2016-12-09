package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Universal extends Formula implements Quantifier{

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
        this.subFormulae = Sets.copy(argument.subFormulae());
        this.variables = argument.variablesPresent();
        this.subFormulae.add(this);
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

            return new Atom("|"+ CommonUtils.sanitizeShadowedString(toString())+"|");

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
        return "(forall " + Arrays.stream(vars).map(Variable::toString).reduce("(", (x,y) -> x + " " +y) + ")" + " "
                + argument.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Universal universal = (Universal) o;

        if (!argument.equals(universal.argument)) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return Arrays.equals(vars, universal.vars);

    }

    @Override
    public int hashCode() {
        int result = argument.hashCode();
        result = 31 * result + Arrays.hashCode(vars);
        return result;
    }
}
