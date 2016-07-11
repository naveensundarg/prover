package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Set;

/**
 * Created by naveensundarg on 7/7/16.
 */
public class Agent extends Constant {


    private String name;

    public Agent(String name) {
        super(name);

    }


    @Override
    public int arity() {
        return 0;
    }

    @Override
    public Value[] getArguments() {
        return new Value[0];
    }

    @Override
    public boolean isVariable() {
        return false;
    }

    @Override
    public boolean isConstant() {
        return true;
    }

    @Override
    public boolean isCompound() {
        return false;
    }

    @Override
    public Set<Variable> variablesPresent() {
        return Sets.newSet();
    }

    @Override
    public Value apply(Map<Variable, Value> substitution) {
        return this;
    }

    @Override
    public Value replace(Value value1, Value value2) {
        return this;
    }
}
