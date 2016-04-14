package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Predicate extends Formula {

    private final String name;
    private final Value[] arguments;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;

    public Predicate(String name){
        this.name = name;
        this.arguments  = new Value[0];
        this.subFormulae = Sets.with(this);
        this.variables = Arrays.stream(arguments).map(Value::variablesPresent).reduce(Sets.newSet(), Sets::union);

    }
    public Predicate(String name, Value[] arguments){
        this.name = name;
        this.arguments = arguments;
        this.subFormulae = Sets.with(this);
        this.variables = Arrays.stream(arguments).map(Value::variablesPresent).reduce(Sets.newSet(), Sets::union);
    }

    public String getName() {
        return name;
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

        Value[] argumentTheta = new Value[arguments.length];

        for(int i = 0; i< argumentTheta.length; i++){
            argumentTheta[i] = arguments[i].apply(substitution);
        }

        return new Predicate(name, argumentTheta);
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return operator.apply(this);
    }

    public Value[] getArguments() {
        return arguments;
    }

    @Override
    public String toString() {
        return name + "(" + Arrays.stream(arguments).map(Value::toString).
                reduce("", (x,y)-> x.isEmpty()? y: x + ", " +y) +")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Predicate predicate = (Predicate) o;

        if (!name.equals(predicate.name)) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return Arrays.equals(arguments, predicate.arguments);

    }


    @Override
    public int hashCode() {
        int result = name.hashCode();
        result = 31 * result + Arrays.hashCode(arguments);
        return result;
    }
}
