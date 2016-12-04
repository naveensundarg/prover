package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 5/4/16.
 */
public class Common extends Formula {

    Value time;
    Formula formula;
    Set<Formula> subFormulae;
    Set<Variable> variables;


    public Common(Value time, Formula formula) {

        this.time = time;
        this.formula = formula;
        this.subFormulae = CollectionUtils.setFrom(formula.subFormulae());
        this.variables = CollectionUtils.setFrom(formula.variablesPresent());

        this.subFormulae.add(this);

        if (time instanceof Variable) {
            variables.add((Variable) time);

        }
    }

    public Formula getFormula(){
        return formula;
    }

    public Value getTime() {
        return time;
    }


    public Set<Variable> getVariables() {
        return variables;
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
        return null;
    }

    @Override
    public Formula shadow(int level) {
        return new Atom("|"+ CommonUtils.sanitizeShadowedString(toString())+"|");
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return null;
    }

    @Override
    public int getLevel() {
        return 2;
    }


    @Override
    public String toString() {
        return "(Common! "
                + time + " "+
                formula + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Common common = (Common) o;

        if (!time.equals(common.time)) return false;
        return formula.equals(common.formula);

    }

    @Override
    public int hashCode() {
        int result = time.hashCode();
        result = 31 * result + formula.hashCode();
        return result;
    }
}
