package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.List;
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



    private final Optional<List<Variable>> answerVariable;
    private final Optional<Set<List<Value>>> answerExpected;
    public Problem(String name, String description, Set<Formula> assumptions, Formula goal) {

        this.assumptions = assumptions;
        this.goal = goal;
        this.name = name;
        this.description = description;
        answerExpected = Optional.empty();

        answerVariable = Optional.empty();

    }

        public Problem(String name, String description, Set<Formula> assumptions, Formula goal,
                       List<Variable> answerVariables, Set<List<Value>> expectedAnswers) {

        this.assumptions = assumptions;
        this.goal = goal;
        this.name = name;
        this.description = description;
        this.answerExpected = Optional.of(expectedAnswers);

        this.answerVariable = Optional.of(answerVariables);

    }

    public Optional<List<Variable>> getAnswerVariables() {
        return answerVariable;
    }

    public Optional<Set<List<Value>>> getAnswersExpected() {
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
