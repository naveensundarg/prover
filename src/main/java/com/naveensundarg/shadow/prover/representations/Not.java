package com.naveensundarg.shadow.prover.representations;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Not extends Formula {

    private final Formula argument;

    private final Set<Formula> subFormuale;
    public Not(Formula argument){
        this.argument = argument;
        this.subFormuale = argument.subFormulae();
    }


    public Formula getArgument() {
        return argument;
    }



    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Not not = (Not) o;

        return argument.equals(not.argument);

    }

    @Override
    public int hashCode() {
        return argument.hashCode();
    }

    @Override
    public String toString() {
        return "(not " + argument +")";
    }

    @Override
    public Set<Formula> subFormulae() {
        return subFormuale;
    }

    @Override
    public Set<Variable> variablesPresent(){
        return argument.variablesPresent();
    }

    @Override
    public Formula apply(Map<Variable, Value> substitution) {
        return new Not(argument.apply(substitution));
    }

    @Override
    public Formula shadow(int level) {
        return new Not(argument.shadow(level));
    }
    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new Not(argument.applyOperation(operator));
    }
}
