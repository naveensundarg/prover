package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 7/24/16.
 */
public class FormulaVariable extends Formula {

    private final String name;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;

    public FormulaVariable(String name) {

        if(!name.startsWith("@")){
            throw new AssertionError("FormulaVariable should start with a @: "+name);
        }

        this.name = name;
        this.subFormulae = Sets.newSet();
        subFormulae.add(this);

        this.variables = Sets.newSet();
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
    public Formula shadow(int level) {
        if(level == 0) {
            return new Atom(this.toString());
        }
        else {
            return this;
        }
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return operator.apply(this);
    }

    @Override
    public int getLevel() {
        return 1;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FormulaVariable that = (FormulaVariable) o;

        return name.equals(that.name);

    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
