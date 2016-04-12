package com.naveensundarg.shadow.prover.representations;

import java.util.Arrays;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Compound extends Value {

    private final Value[] arguments;
    private final String name;

    public Compound(String name, Value[] arguments){
        super();
        this.arguments = arguments;
        this.name = name;

    }

    @Override
    int arity() {
        return arguments.length;
    }

    @Override
    Value[] getArguments() {
        return Arrays.copyOf(arguments, arguments.length);
    }

    @Override
    boolean isVariable() {
        return false;
    }

    @Override
    boolean isConstant() {
        return false;
    }

    @Override
    boolean isCompound() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Compound compound = (Compound) o;

        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(arguments, compound.arguments)) return false;
        return name.equals(compound.name);

    }

    @Override
    public int hashCode() {
        int result = Arrays.hashCode(arguments);
        result = 31 * result + name.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return name + "(" +Arrays.toString(arguments) +")";
    }
}
