package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.List;
import java.util.Optional;
import java.util.Set;


public class DPLChunk  {

    private final Set<Formula> assumptions;

    private final Phrase goal;

    private final String name;
    private final String description;

    private final Phrase input;


    private final Optional<List<Variable>> answerVariable;
    private final Optional<Set<List<Value>>> answerExpected;
    public DPLChunk(String name, String description, Set<Formula> assumptions, Phrase input, Phrase goal) {

        this.assumptions = assumptions;
        this.goal = goal;
        this.name = name;
        this.description = description;
        this.input = input;

        answerExpected = Optional.empty();

        answerVariable = Optional.empty();

    }

        public DPLChunk(String name, String description, Set<Formula> assumptions, Phrase input, Formula goal,
                       List<Variable> answerVariables, Set<List<Value>> expectedAnswers) {

        this.assumptions = assumptions;
        this.goal = goal;
        this.name = name;
        this.description = description;
        this.answerExpected = Optional.of(expectedAnswers);
        this.input = input;

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

    public Phrase getGoal() {
        return goal;
    }

    public String getName() {
        return name;
    }

    public Phrase getInput() {
        return input;
    }

    public String getDescription() {
        return description;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DPLChunk dplChunk = (DPLChunk) o;

        if (assumptions != null ? !assumptions.equals(dplChunk.assumptions) : dplChunk.assumptions != null)
            return false;
        if (goal != null ? !goal.equals(dplChunk.goal) : dplChunk.goal != null) return false;
        if (name != null ? !name.equals(dplChunk.name) : dplChunk.name != null) return false;
        if (description != null ? !description.equals(dplChunk.description) : dplChunk.description != null)
            return false;
        if (answerVariable != null ? !answerVariable.equals(dplChunk.answerVariable) : dplChunk.answerVariable != null)
            return false;
        return answerExpected != null ? answerExpected.equals(dplChunk.answerExpected) : dplChunk.answerExpected == null;
    }

    @Override
    public int hashCode() {
        int result = assumptions != null ? assumptions.hashCode() : 0;
        result = 31 * result + (goal != null ? goal.hashCode() : 0);
        result = 31 * result + (name != null ? name.hashCode() : 0);
        result = 31 * result + (description != null ? description.hashCode() : 0);
        result = 31 * result + (answerVariable != null ? answerVariable.hashCode() : 0);
        result = 31 * result + (answerExpected != null ? answerExpected.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "DPLChunk{" +
                "assumptions=" + assumptions +
                ", goal=" + goal +
                ", name='" + name + '\'' +
                ", description='" + description + '\'' +
                ", answerVariable=" + answerVariable +
                ", answerExpected=" + answerExpected +
                '}';
    }
}
