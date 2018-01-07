package com.naveensundarg.shadow.prover.representations.value;

import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Compound extends Value {

    private final Value[] arguments;
    private final Set<Variable> variables;
    private final Set<Value> subValues;
    private final int weight;


    public Compound(String name, List<Value> argumentsList){
        super();
        this.arguments = new Value[argumentsList.size()];
        super.name = name;
        for(int i = 0; i<argumentsList.size(); i++){
            arguments[i] = argumentsList.get(i);
        }
        this.variables = Arrays.stream(arguments).map(Value::variablesPresent).reduce(Sets.newSet(), Sets::union);

        this.subValues = Arrays.stream(arguments).map(Value::subValues).reduce(Sets.newSet(), Sets::union);
      this.subValues().add(this);

        this.weight = argumentsList.stream().mapToInt(Value::getWeight).reduce(0, (x, y)-> x + y) + 1;
    }
    public Compound(String name, Value[] arguments){
        super();
        this.arguments = arguments;
        super.name = name;
        this.variables = Arrays.stream(arguments).map(Value::variablesPresent).reduce(Sets.newSet(), Sets::union);
        this.subValues = Arrays.stream(arguments).map(Value::subValues).reduce(Sets.newSet(), Sets::union);
       this.subValues().add(this);
        this.weight = Arrays.stream(arguments).mapToInt(Value::getWeight).reduce(0, (x, y)-> x + y) + 1;

    }

    @Override
    public boolean occurs(Variable variable){

        return Arrays.stream(arguments).anyMatch(x-> {

                    if (x.equals(variable)) {
                        return true;
                    }

                    if (!x.isCompound()) {
                        return false;
                    } else {
                        return x.occurs(variable);
                    }

                }
        );

    }

    @Override
    public Set<Variable> variablesPresent() {
        return variables;
    }

    @Override
    public Value apply(Map<Variable, Value> substitution) {
        Value[] argumentTheta = new Value[arguments.length];

        for(int i = 0; i< argumentTheta.length; i++){
            argumentTheta[i] = arguments[i].apply(substitution);
        }

        return new Compound(name, argumentTheta);
    }

    @Override
    public Value replace(Value value1, Value value2) {

        if(value1.equals(this)){
            return value2;
        }

        Value[] argumentTheta = new Value[arguments.length];

        for(int i = 0; i< argumentTheta.length; i++){
            argumentTheta[i] = arguments[i].replace(value1, value2);
        }

        return new Compound(name, argumentTheta);


    }

    public  Value generalize(Map<Value, Variable> substitution){


        if(substitution.containsKey(this)){

            return substitution.get(this);
        }

        Value[] argumentTheta = new Value[arguments.length];

        for(int i = 0; i< argumentTheta.length; i++){


                argumentTheta[i] = arguments[i].generalize(substitution);


        }

        return new Compound(name, argumentTheta);

    }


    @Override
    public Set<Value> subValues() {
        return subValues;
    }

    @Override
    public int getWeight() {
        return weight;
    }

    @Override
    public Optional<Pair<Variable, Value>> subsumes(Value other) {
        return Optional.empty();
    }

    @Override
    public  int arity() {
        return arguments.length;
    }

    @Override
    public Value[] getArguments() {
        return Arrays.copyOf(arguments, arguments.length);
    }

    @Override
    public boolean isVariable() {
        return false;
    }

    @Override
    public  boolean isConstant() {
        return false;
    }

    @Override
    public  boolean isCompound() {
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
        result = 31 * result + safeHashCode(name);
        return result;
    }

    @Override
    public String toString() {
        return "("+ name +" " + Arrays.stream(arguments).map(Value::toString).reduce("", (x,y) -> x + " " +y) + ")"  ;
    }

    @Override
    public int compareTo(Object o) {
        return 0;
    }
}
