package com.naveensundarg.shadow.prover.representations.formula;

import clojure.lang.Compiler;
import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Reader;
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
    private final boolean isStringName; // e.g. "It is raining";
    public Atom(String name){
        super(name);
        this.name = name;

        this.subFormulae = Sets.newSet();
        subFormulae.add(this);

        this.variables = Sets.newSet();
        this.boundVariables = Sets.newSet();

        //TODO: Check if
        this.isStringName = false;
    }

    public Atom(String name, boolean isStringName){
        super(name);
        this.name = name;

        this.subFormulae = Sets.newSet();
        subFormulae.add(this);

        this.variables = Sets.newSet();
        this.boundVariables = Sets.newSet();

        this.isStringName = isStringName;
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {

        if(!isStringName) {
            return name;
        } else {
            return "\"" + name + "\"";
        }
    }

    @Override
    public String toSnarkString(){

        if(!isStringName) {
            return name;
        } else {
            return "\"" + name.replaceAll(" ", "_") + "\"";
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if(o instanceof Predicate){
            return  ((Predicate) o).getArguments().length == 0 && name.equals(((Predicate) o).getName());
        }
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

        Variable varName = new Variable(name);
        if (substitution.containsKey(varName)) {
            if (!(substitution.get(varName) instanceof Compound)) {
                return new Atom(substitution.get(varName).toString());
            } else {
                try {
                    return Reader.readFormulaFromString(substitution.get(varName).toString());
                } catch (Reader.ParsingException e) {
                    return Logic.getTrueFormula();
                }

            }
        } else {
            return this;

        }
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
    public Formula generalize(Map<Value, Variable> substitution) {

        String newName = substitution.getOrDefault(new Constant(name), new Variable(name)).getName();

        return new Atom(newName);
    }
    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }
}
