package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.Expression;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Reader;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Created by naveensundarg on 4/8/16.
 */
public abstract class Formula extends Expression {




    private Justification justification;


    public Justification getJustification() {
        return justification;
    }

    public Formula setJustification(Justification justification) {
        this.justification = justification;

        return this;
    }

    public abstract Set<Formula> subFormulae();

    public abstract Set<Variable> variablesPresent();

    public abstract Formula apply(Map<Variable, Value> substitution);

    public  Formula replace(Value oldValue, Value newValue){

        try {
            return Reader.readFormulaFromString(this.toString().replace(oldValue.toString(), newValue.toString()));
        } catch (Reader.ParsingException e) {
            return this;
        }
    }

    public  Formula generalize(Map<Value, Variable> substitution){

        throw new NotImplementedException();
    }


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
