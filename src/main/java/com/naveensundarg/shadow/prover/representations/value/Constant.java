package com.naveensundarg.shadow.prover.representations.value;

import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Constant extends Value implements Comparable{



    private final Value[] arguments;
    private final Set<Variable> variables;
    private final Set<Value> subValues;

    public Constant(String name){

        super();
        this.arguments = new Value[0];
        super.name = name;
        this.variables = Sets.newSet();
        this.subValues = Sets.with(this);
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
    public Value replace(Value value1, Value value2) {
        return value1.equals(this)? value2 : this;
    }

    @Override
    public  Value generalize(Map<Value, Variable> substitution) {
         if(substitution.containsKey(this)){

            return substitution.get(this);
        }
        else {

             return this;
         }

    }
    @Override
    public Set<Value> subValues() {
        return subValues;
    }

    @Override
    public int getWeight() {
        return 1;
    }

    @Override
    public Optional<Pair<Variable, Value>> subsumes(Value other) {
        return Optional.empty();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Constant constant = (Constant) o;

        return name.equals(constant.getName());

    }

    @Override
    public int hashCode() {
        int result = Arrays.hashCode(arguments);
        result = variables != null ? 31 * result + safeHashCode(variables) : result;
        return result;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public int compareTo(Object o) {
        if(!(o instanceof  Constant)) {
            return 0;
        }
        else {
            Constant other = (Constant) o;

            return this.name.compareTo(((Constant) o).name);

        }
    }

    private static AtomicInteger counter = new AtomicInteger(0);
    public synchronized static Constant newConstant(){

        return new Constant("newC_" + counter.incrementAndGet());



    }
}
