package com.naveensundarg.shadow.prover.representations;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 5/4/16.
 */
public class Belief extends Formula implements Comparable<Belief>{

    Agent agent;
    int time;
    int strength;
    Formula formula;

    public Belief(Agent agent, int time, Formula formula, int strength){
        if(strength < 0){
            throw new AssertionError("Strenght of a belief should be a positive integer: B("
                    + agent + ", " + time + ", " + formula + ", " + strength +")");
        }

        this.agent = agent;
        this.time = time;
        this.formula = formula;
        this.strength = strength;

    }

    public Belief(Agent agent, int time, Formula formula){
        this.agent = agent;
        this.time = time;
        this.formula = formula;
        this.strength = 1;
    }
    @Override
    public Set<Formula> subFormulae() {
        return null;
    }

    @Override
    public Set<Variable> variablesPresent() {
        return null;
    }

    @Override
    public Formula apply(Map<Variable, Value> substitution) {
        return null;
    }

    @Override
    public Formula shadow(int level) {
        return null;
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return null;
    }

    @Override
    public int compareTo(Belief o) {
        return strength - o.strength;
    }
}
