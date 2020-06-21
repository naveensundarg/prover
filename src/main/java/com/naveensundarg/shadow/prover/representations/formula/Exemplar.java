package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.UnaryOperator;

public class Exemplar extends BaseFormula {

    private final Formula input;
    private final Formula output;

    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;

    private final Set<Variable> boundVariables;

    private final Set<Value> allValues;

    private final int weight;

    public Exemplar(Formula input, Formula output) {


        this.input = input;
        this.subFormulae = CollectionUtils.setFrom(input.subFormulae());
        this.output = output;
        this.subFormulae.add(this);
        this.allValues = Sets.newSet();

        this.variables  = Sets.union(input.variablesPresent(), output.variablesPresent());
        this.values  = Sets.union(input.valuesPresent(), output.valuesPresent());
        this.boundVariables =  Sets.union(input.boundVariablesPresent(), output.boundVariablesPresent());



        this.weight = 1 +  this.input.getWeight() + output.getWeight();
    }

    public Formula getInput(){
        return input;
    }


    public Formula getOutput() {
        return output;
    }



    public Set<Formula> getSubFormulae() {
        return subFormulae;
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
        return new Exemplar(input.apply(substitution), output.apply(substitution));
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
    public int getWeight() {
        return weight;
    }

    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new Exemplar(input.replaceSubFormula(oldFormula, newFormula), output.replaceSubFormula(oldFormula, newFormula));
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }

    @Override
    public Set<Value> valuesPresent() {
        return values;
    }


    @Override
    public String toString() {
        return "(Exemplar! " +
                input + " " +
                output + ")";
    }

    @Override
    public String toSnarkString() {
        return "(Exemplar! " +
                input.toSnarkString() + " " +
                output.toSnarkString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Exemplar exemplar = (Exemplar) o;
        return Objects.equals(input, exemplar.input) &&
                Objects.equals(output, exemplar.output);
    }

    @Override
    public int hashCode() {
        return Objects.hash(input, output);
    }

    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return "Exemplar";
    }
}
