package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.Formula;

import java.util.Set;

/**
 * Created by naveensundarg on 4/13/16.
 */
public class Problem {


    private final Set<Formula> assumptions;

    private final Formula goal;

    public Problem(Set<Formula> assumptions, Formula goal) {

        this.assumptions = assumptions;
        this.goal = goal;
    }

    public Set<Formula> getAssumptions() {
        return assumptions;
    }

    public Formula getGoal() {
        return goal;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Problem problem = (Problem) o;

        if (!assumptions.equals(problem.assumptions)) return false;
        return goal.equals(problem.goal);

    }

    @Override
    public int hashCode() {
        int result = assumptions.hashCode();
        result = 31 * result + goal.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Problem{" +
                "assumptions=" + assumptions +
                ", goal=" + goal +
                '}';
    }
}
