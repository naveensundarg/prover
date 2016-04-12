package com.naveensundarg.shadow.prover.representations;

import java.util.Arrays;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Constant extends Value {

    private final Value[] arguments;
    private final String name;

    public Constant(String name){

        super();
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
        return false;
    }

    @Override
    boolean isConstant() {
        return true;
    }

    @Override
    boolean isCompound() {
        return false;
    }

    @Override
    public String toString() {
        return name;
    }
}
