package com.naveensundarg.shadow.prover.representations;

/**
 * Created by naveensundarg on 4/11/16.
 */
public abstract class Value {

    public Value(){

        if(!(isCompound()|| isConstant() || isVariable())){
            throw new AssertionError();
        }
    }

    abstract int arity();
    abstract Value[] getArguments();

    abstract boolean isVariable();
    abstract boolean isConstant();
    abstract boolean isCompound();


 }
