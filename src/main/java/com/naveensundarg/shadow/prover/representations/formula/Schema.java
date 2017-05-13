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
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/29/17.
 */
public class Schema extends Formula {


    private final Formula argument;
    private final Variable[] vars;
    private Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;

    private final Set<Variable> boundVariables;
    private final int weight;

    public Schema(Variable[] vars, Formula argument) {

        if (!(vars.length > 0)) {
            throw new AssertionError("Schema should have at least one variable");
        }

        this.vars = vars;
        this.argument = argument;
        this.subFormulae = Sets.copy(argument.subFormulae());
        this.variables = argument.variablesPresent();
        this.values = Sets.union(Arrays.stream(vars).collect(Collectors.toSet()),  argument.valuesPresent());
        this.boundVariables = Sets.union(Arrays.stream(vars).collect(Collectors.toSet()), argument.boundVariablesPresent());
        this.subFormulae.add(this);
        Arrays.stream(vars).forEach(this.variables::add);

        this.weight = 1 + variables.stream().mapToInt(Value::getWeight).reduce(0, Integer::sum)  + argument.getWeight();

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

    @Override
    public int getWeight() {
        return weight;
    }

    public Variable[] vars() {
        return vars;
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
        return "(schema " + "(" + StringUtils.trim(Arrays.stream(vars).map(Variable::toString).reduce("", (x, y) -> x  + y + " "))
                + ")" + " "
                + argument.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Schema schema = (Schema) o;

        if (!argument.equals(schema.argument)) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return Arrays.equals(vars, schema.vars);
    }

    @Override
    public int hashCode() {
        int result = argument.hashCode();
        result = 31 * result + Arrays.hashCode(vars);
        return result;
    }

    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        return new Universal(vars, argument.replaceSubFormula(oldFormula, newFormula));
    }
}
