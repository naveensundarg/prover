package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

public class Inference extends Formula {

    private final Formula[] antecedents;
    private final Formula conclusion;

    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;

    private final Set<Variable> boundVariables;
    private final int level;
    private final int weight;

    public Inference(Formula[] antecedents, Formula conclusion) {
        this.antecedents = antecedents;
        this.conclusion = conclusion;
        this.subFormulae = Sets.union(Arrays.stream(antecedents).map(Formula::subFormulae).
                reduce(Sets.newSet(), Sets::union), conclusion.subFormulae());
        this.subFormulae.add(this);
        this.variables = Sets.union(Arrays.stream(antecedents).map(Formula::variablesPresent).reduce(Sets.newSet(), Sets::union)
                , conclusion.variablesPresent());
        this.values = Sets.union(Arrays.stream(antecedents).map(Formula::valuesPresent).reduce(Sets.newSet(), Sets::union), conclusion.valuesPresent());
        this.boundVariables = Sets.union(Arrays.stream(antecedents).map(Formula::boundVariablesPresent).reduce(Sets.newSet(), Sets::union), boundVariablesPresent());

        this.level = Math.max(CommonUtils.maxLevel(antecedents), conclusion.getLevel());
        this.weight = 1 + Arrays.stream(antecedents).mapToInt(Formula::getWeight).reduce(0, Integer::sum) + conclusion.getWeight();


    }

    public Inference(List<Formula> antecedents, Formula conclusion) {
        this.antecedents = new Formula[antecedents.size()];
        this.conclusion = conclusion;

        for (int i = 0; i < antecedents.size(); i++) {
            this.antecedents[i] = antecedents.get(i);
        }
        this.subFormulae = Sets.union(Arrays.stream(this.antecedents).map(Formula::subFormulae).
                reduce(Sets.newSet(), Sets::union), conclusion.subFormulae());

        this.subFormulae.add(this);
        this.variables = Sets.union(Arrays.stream(this.antecedents).map(Formula::variablesPresent).reduce(Sets.newSet(), Sets::union)
                , conclusion.variablesPresent());
        this.values = Sets.union(Arrays.stream(this.antecedents).map(Formula::valuesPresent).reduce(Sets.newSet(), Sets::union), conclusion.valuesPresent());
        this.boundVariables = Sets.union(Arrays.stream(this.antecedents).map(Formula::boundVariablesPresent).reduce(Sets.newSet(), Sets::union), boundVariablesPresent());

        this.level = Math.max(CommonUtils.maxLevel(this.antecedents), conclusion.getLevel());
        this.weight = 1 + Arrays.stream(this.antecedents).mapToInt(Formula::getWeight).reduce(0, Integer::sum) + conclusion.getWeight();


    }

    public Formula[] getAntecedents() {
        return antecedents.clone();
    }

    public Formula getConclusion() {
        return conclusion;
    }

    @Override
    public String toString() {
        return "(implies " + CommonUtils.toString(antecedents) + " " + conclusion.toString() + ")";
    }

    @Override
    public String toSnarkString() {
        return "(implies " + CommonUtils.toSnarkString(antecedents) + " " + conclusion.toSnarkString() + " )";
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
    public Set<Value> valuesPresent() {
        return values;
    }


    @Override
    public Formula apply(Map<Variable, Value> substitution) {
        return new Inference(Arrays.stream(antecedents).map(x -> x.apply(substitution)).collect(Collectors.toList()),
                conclusion.apply(substitution));

    }

    @Override
    public Formula generalize(Map<Value, Variable> substitution) {
        return new Inference(Arrays.stream(antecedents).map(x -> x.generalize(substitution)).collect(Collectors.toList()),
                conclusion.generalize(substitution));

    }

    @Override
    public Formula shadow(int level) {
        return new Inference(Arrays.stream(antecedents).map(f -> f.shadow(level)).collect(Collectors.toList()),
                conclusion.shadow(level));
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new Inference(Arrays.stream(antecedents).map(x -> x.applyOperation(operator)).collect(Collectors.toList()),
                conclusion.applyOperation(operator));

    }

    @Override
    public int getLevel() {
        return level;
    }

    @Override
    public int getWeight() {
        return weight;
    }

    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {

        if (oldFormula.equals(this)) {

            return newFormula;
        }

        if (!subFormulae().contains(oldFormula)) {

            return this;
        }

        Formula[] newArgs = new Formula[antecedents.length];

        for (int i = 0; i < antecedents.length; i++) {

            newArgs[i] = antecedents[i].replaceSubFormula(oldFormula, newFormula);
        }

        return new Inference(newArgs, conclusion.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }
}
