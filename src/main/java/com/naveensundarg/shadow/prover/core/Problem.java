package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import javax.swing.text.html.Option;
import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 4/13/16.
 */
public class Problem {


    private final Set<Formula> assumptions;

    private final Formula goal;

    private final String name;
    private final String description;



    private final Optional<Variable> answerVariable;
    private final Optional<Value> answerExpected;
    public Problem(String name, String description, Set<Formula> assumptions, Formula goal) {

        this.assumptions = assumptions;
        this.goal = goal;
        this.name = name;
        this.description = description;
        answerExpected = Optional.empty();

        answerVariable = Optional.empty();

    }

        public Problem(String name, String description, Set<Formula> assumptions, Formula goal,
                       Variable answerVariable, Value expectedAnswer) {

        this.assumptions = assumptions;
        this.goal = goal;
        this.name = name;
        this.description = description;
        this.answerExpected = Optional.of(expectedAnswer);

        this.answerVariable = Optional.of(answerVariable);

    }

     public Optional<Variable> getAnswerVariable() {
        return answerVariable;
    }

    public Optional<Value> getAnswerExpected() {
        return answerExpected;
    }

    public Set<Formula> getAssumptions() {
        return assumptions;
    }

    public Formula getGoal() {
        return goal;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
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
