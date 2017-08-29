package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.Expression;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/8/16.
 */
public abstract class Formula extends Expression {



    public abstract Set<Formula> subFormulae();

    public abstract Set<Variable> variablesPresent();

    public abstract Formula apply(Map<Variable, Value> substitution);

    public abstract Formula shadow(int level);

    public abstract Formula applyOperation(UnaryOperator<Formula> operator);

    public abstract int getLevel();

    public abstract int getWeight();

    public abstract Formula replaceSubFormula(Formula oldFormula, Formula newFormula);

    public abstract Set<Variable> boundVariablesPresent();

    public abstract Set<Value> valuesPresent();

    public static String _getSlateString_(Formula formula){

        return formula.toString().replace("implies ", "if ");
    }
}
