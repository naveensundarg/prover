package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.Expression;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/8/16.
 */
public abstract class Formula extends Expression {




    private Justification justification;
    private Set<Formula>  assumptions;
    private int           strengthFactor;

    public Formula(){
        this.strengthFactor = Integer.MAX_VALUE;
    }

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

        throw new NotImplementedException("generalize");
    }

    public Set<Formula> getAssumptions() {
        return assumptions;
    }

    public void setAssumptions(Set<Formula> assumptions) {
        this.assumptions = assumptions;
    }

    public abstract Formula shadow(int level);

    public abstract Formula applyOperation(UnaryOperator<Formula> operator);

    public abstract int getLevel();

    public abstract int getWeight();

    public int getStrengthFactor(){
        return strengthFactor;
    }

    public void setStrengthFactor(int strength){
        this.strengthFactor = strength;
    }

    public abstract Formula replaceSubFormula(Formula oldFormula, Formula newFormula);

    public abstract Set<Variable> boundVariablesPresent();

    public abstract Set<Value> valuesPresent();

    public static String _getSlateString_(Formula formula){

        return formula.toString().replace("implies ", "if ");
    }

    public abstract String toSnarkString();

    public Set<Variable> freeVariablesPresent() {

        Set<Variable> freeVariables = this.valuesPresent().stream().
                filter(value -> value instanceof Variable).
                map(value -> (Variable) value).
                collect(Collectors.toSet());

        return Sets.difference(freeVariables, this.boundVariablesPresent());
    }
    public Set<Constant> constantsPresent() {

        return this.valuesPresent().stream().
                filter(value -> value instanceof Constant).
                map(c -> (Constant) c).
                collect(Collectors.toSet());

    }

    public int maxPredicateArity() {
        return this.subFormulae().stream().
                filter(f-> f instanceof Predicate).
                map(f -> (Predicate) f).
                mapToInt(p -> p.getArguments().length).
                max().orElse(0);
    }


    public int maxFunctionArity() {
        return this.valuesPresent().stream().
                filter(v-> v instanceof Compound).
                map(c -> (Compound) c).
                mapToInt(c -> c.getArguments().length).
                max().orElse(0);
    }

}
