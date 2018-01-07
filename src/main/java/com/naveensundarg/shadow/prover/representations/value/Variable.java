package com.naveensundarg.shadow.prover.representations.value;

import com.naveensundarg.shadow.prover.utils.ImmutablePair;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Variable extends Value {

    private final Value[] arguments;
    private final Set<Variable> variables;
    private final Set<Value> subValues;
    private final int id;

    private static final boolean variablesShouldStartWithQuestionMark = false;
    public Variable(String name) {

        super();

        if (variablesShouldStartWithQuestionMark && !name.startsWith("?")) {
            throw new AssertionError("Variables should start with ?: " + name);
        }

        this.arguments = new Value[0];
        super.name = name;

        this.variables = Sets.with(this);
        this.subValues = Sets.with(this);
        this.id = setId();

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
        return value1.equals(this) ? value2 : this;
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

        if (other.subValues().contains(this)) {

            return Optional.empty();

        } else {

            return Optional.of(ImmutablePair.from(this, other));

        }
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
        return safeHashCode(name);
    }

    @Override
    public int compareTo(Object o) {
        if (!(o instanceof Variable)) {
            return 0;
        } else {
            Variable other = (Variable) o;

            return this.name.compareTo(((Variable) o).name);

        }
    }

    public int getId() {
        return id;
    }

    private int setId() {

        if (name.matches("\\?x[0-9]+")) {

            return Integer.parseInt(name.substring(2));


        } else {

            return -1;

        }
    }
}
