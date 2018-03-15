package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Predicate extends BaseFormula {

    private final String name;
    private final Value[] arguments;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;
    private final Set<Variable> boundVariables;
    private final boolean sorted;
    private final Set<Value> allValues;

    private static final AtomicBoolean orientEqualities = new AtomicBoolean(false);

    public static void setOrientEqualities(boolean val){

        orientEqualities.set(val);
    }
    private final int weight;
    public Predicate(String name){

        this.sorted = false;
        this.name = name;
        this.arguments  = new Value[0];
        this.subFormulae = Sets.with(this);
        this.variables = Arrays.stream(arguments).map(Value::variablesPresent).reduce(Sets.newSet(), Sets::union);
        this.values = Arrays.stream(arguments).map(Value::subValues).reduce(Sets.newSet(), Sets::union);

        this.boundVariables = Sets.newSet();
        this.allValues = Arrays.stream(arguments).map(Value::subValues).reduce(Sets.newSet(), Sets::union);

        this.weight = 1;

    }
    public Predicate(String name, Value[] arguments){

        this.sorted = false;
        this.name = name;
        if(name.equals("=") && orientEqualities.get()){

            List<Value> sortedArgs = Arrays.stream(arguments).sorted(Comparator.comparingInt(x->-x.getWeight())).collect(Collectors.toList());

            this.arguments = new Value[sortedArgs.size()];

            for(int i = 0; i<sortedArgs.size(); i++){

                this.arguments[i]  = sortedArgs.get(i);
            }


        } else {
            this.arguments = arguments;
        }

        this.subFormulae = Sets.with(this);
        this.variables = Arrays.stream(arguments).map(Value::variablesPresent).reduce(Sets.newSet(), Sets::union);
        this.values = Arrays.stream(arguments).map(Value::subValues).reduce(Sets.newSet(), Sets::union);

        this.boundVariables = Sets.newSet();

        this.allValues = Arrays.stream(arguments).map(Value::subValues).reduce(Sets.newSet(), Sets::union);

        this.weight = 1 + Arrays.stream(arguments).mapToInt(Value::getWeight).reduce(0, Integer::sum);


    }

    private static void validateName(String name){
        if(name.startsWith("$") || name.endsWith("$")){

            throw new AssertionError("Invalid name: "+name);
        }
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
    public Formula generalize(Map<Value, Variable> substitution) {


        Value[] newArguments = new Value[arguments.length];

        for(int i = 0; i<arguments.length; i++){

            newArguments[i] = (arguments[i].generalize(substitution));


        }
        return new Predicate(name, newArguments);
    }

    public Set<Value> allValues() {

        return allValues;

    }

    @Override
    public Formula shadow(int level) {
        if(level == 1){
            return this;
        }
        else if (level == 0){
          return new Atom("|"+ CommonUtils.sanitizeShadowedString(toString())+"|");
        }

        throw new AssertionError("Not a valid getLevel for shadowing: "+level);
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return operator.apply(this);
    }

    @Override
    public int getLevel() {
        return 1;
    }

    @Override
    public int getWeight() {
        return weight;
    }

    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        return (oldFormula.equals(this))? newFormula : this;
    }

    public Value[] getArguments() {
        return arguments;
    }

    @Override
    public String toString() {
        return "(" + name + " "+ Arrays.stream(arguments).map(Value::toString).
                reduce("", (x,y)-> x.isEmpty()? y: x + " " +y) +")";
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
        int result = safeHashCode(name);
        result = 31 * result + Arrays.toString(arguments).hashCode();
        return result;
    }



    public Predicate replace(Value value1, Value value2){
        Value[] argumentTheta = new Value[arguments.length];

        for(int i = 0; i< argumentTheta.length; i++){
            argumentTheta[i] = arguments[i].replace(value1, value2);
        }

        return new Predicate(name, argumentTheta);

    }


    public Optional<Map<Variable, Value>> subsumes(Predicate other){


        if(!other.getName().equals(name)){

            return Optional.empty();

        } else {

            return Value.subsumes(this.getArguments(), other.getArguments());

        }


    }

    @Override
    public Set<Variable> boundVariablesPresent() {

        return boundVariables;
    }

    @Override
    public Set<Value> valuesPresent() {
        return values;
    }
}
