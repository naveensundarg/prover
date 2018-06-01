package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 5/4/16.
 */
public final class GeneralModal extends  BaseFormula{

    private Value name;
    private final List<Formula> formulae;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;
    private final Set<Value> values;
    private final Set<Variable> boundVariables;
    private final Set<Value> allValues;

    private final int weight;



    public GeneralModal(Value name, List<Formula> formulae) {

        this.name = name;
        this.formulae = formulae;
        this.subFormulae = CollectionUtils.setFrom(formulae.stream().map(Formula::subFormulae).reduce(Sets.newSet(), Sets::union));
        this.variables = CollectionUtils.setFrom(formulae.stream().map(Formula::variablesPresent).reduce(Sets.newSet(), Sets::union));
        this.values = CollectionUtils.setFrom(formulae.stream().map(Formula::valuesPresent).reduce(Sets.newSet(), Sets::union));

        this.boundVariables = CollectionUtils.setFrom(formulae.stream().map(Formula::boundVariablesPresent).reduce(Sets.newSet(), Sets::union));
        this.allValues = Sets.newSet();

        this.subFormulae.add(this);


        this.weight =   formulae.stream().mapToInt(Formula::getWeight).sum();
    }

    public static GeneralModal build(String name, Formula... formulae) {

        return new GeneralModal( new Constant(name), Arrays.stream(formulae).collect(Collectors.toList()));
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
    public Set<Value> valuesPresent() {
        return values;
    }


    @Override
    public Formula apply(Map<Variable, Value> substitution) {
        return   new GeneralModal(name, formulae.stream().map(f->f.apply(substitution)).collect(Collectors.toList()) );

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

    public List<Formula> getFormulae() {
        return formulae;
    }

    @Override
    public String toString() {
        return "(" + name
                +
                formulae.stream().map(Object::toString).reduce(" ", (x, y)-> x + " " +y) + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GeneralModal that = (GeneralModal) o;
        return Objects.equals(name, that.name) &&
                Objects.equals(formulae, that.formulae);
    }

    @Override
    public int hashCode() {

        return Objects.hash(name, formulae);
    }

    @Override
    public Set<Value> allValues() {
        return allValues;
    }

    @Override
    public String getName() {
        return name.getName();
    }



    @Override
    public Formula replaceSubFormula(Formula oldFormula, Formula newFormula) {
        if(oldFormula.equals(this)){

            return newFormula;
        }

        if(!subFormulae().contains(oldFormula)){

            return this;
        }


        return new GeneralModal(name, formulae.stream().map(f->f.replaceSubFormula(oldFormula, newFormula)).collect(Collectors.toList()));
    }

    @Override
    public Set<Variable> boundVariablesPresent() {
        return boundVariables;
    }


}
