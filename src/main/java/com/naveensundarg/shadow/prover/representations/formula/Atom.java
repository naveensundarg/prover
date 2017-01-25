package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/8/16.
 */
public final class Atom extends Predicate{

    private final String name;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Variable> boundVariables;
    public Atom(String name){
        super(name);
        this.name = name;

        this.subFormulae = Sets.newSet();
        subFormulae.add(this);

        this.variables = Sets.newSet();
        this.boundVariables = Sets.newSet();
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Atom atom = (Atom) o;

        return name != null ? name.equals(atom.name) : atom.name == null;

    }

    @Override
    public int hashCode() {
        return safeHashCode(name);
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
        return this;
    }

    @Override
    public Atom shadow(int level) {
        return this;
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return operator.apply(this);
    }

    @Override
    public int getLevel() {
        return 0;
    }

    private static int generatedCount = 0;
    public static Atom generate(){

        generatedCount++;
        return new Atom("$Gen_"+generatedCount+"$");


    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }
}
