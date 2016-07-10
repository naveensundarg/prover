package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Variable extends Value {

    private final Value[] arguments;
    private final Set<Variable> variables;

    public Variable(String name){

        super();
        if(!name.startsWith("?")){
            throw new AssertionError("Variables should start with ?: "+ name);
        }

        this.arguments = new Value[0];
        super.name = name;

        this.variables = Sets.with(this);

    }

    @Override
    public int arity() {
        return 0;
    }

    @Override
    public Value[] getArguments() {
        return arguments;
    }

    @Override
    public boolean isVariable() {
        return true;
    }

    @Override
    public boolean isConstant() {
        return false;
    }

    @Override
    public boolean isCompound() {
        return false;
    }

    @Override
    public Set<Variable> variablesPresent() {
        return variables;
    }

    @Override
    public Value apply(Map<Variable, Value> substitution) {
        return substitution.getOrDefault(this, this);
    }

    @Override
    public Value replace(Value value1, Value value2) {
        return value1.equals(this)? value2 : this;
    }

    @Override
    public String toString() {
        return name;
    }

    public String getName() {
        return name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Variable variable = (Variable) o;

        return name.equals(variable.name);

    }


    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
