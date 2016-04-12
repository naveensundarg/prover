package com.naveensundarg.shadow.prover.representations;

import java.util.Arrays;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Variable extends Value{

    private final Value[] arguments;
    private final String name;

    public Variable(String name){

        super();
        if(!name.startsWith("?")){
            throw new AssertionError("Variables should start with ?: "+ name);
        }

        this.arguments = null;
        this.name = name;

    }

    @Override
    int arity() {
        return 0;
    }

    @Override
    Value[] getArguments() {
        return arguments;
    }

    @Override
    boolean isVariable() {
        return true;
    }

    @Override
    boolean isConstant() {
        return false;
    }

    @Override
    boolean isCompound() {
        return false;
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
