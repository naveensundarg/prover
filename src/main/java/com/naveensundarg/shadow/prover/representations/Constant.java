package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Constant extends Value {

    private final Value[] arguments;
    private final Set<Variable> variables;
    public Constant(String name){

        super();
        this.arguments = null;
        super.name = name;
        this.variables = Sets.newSet();

    }
    @Override
    public  int arity() {
        return 0;
    }

    @Override
    public  Value[] getArguments() {
        return arguments;
    }

    @Override
    public  boolean isVariable() {
        return false;
    }

    @Override
    public  boolean isConstant() {
        return true;
    }

    @Override
    public  boolean isCompound() {
        return false;
    }

    @Override
    public Set<Variable> variablesPresent() {
        return variables;
    }

    @Override
    public Value apply(Map<Variable, Value> substitution) {
        return this;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Constant constant = (Constant) o;

        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(arguments, constant.arguments)) return false;
        return variables.equals(constant.variables);

    }

    @Override
    public int hashCode() {
        int result = Arrays.hashCode(arguments);
        result = 31 * result + variables.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return name;
    }
}
