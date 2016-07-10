package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Common;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class And extends Formula {

    private final Formula[] arguments;
    private final Set<Formula> subFormulae;
    private final Set<Variable> variables;

    public And(Formula ... arguments){
        this.arguments = arguments;
        this.subFormulae = Arrays.stream(arguments).map(Formula::subFormulae).
                reduce(Sets.newSet(), Sets::union);

        this.variables = Arrays.stream(arguments).map(Formula::variablesPresent).reduce(Sets.newSet(), Sets::union);
    }

    public And(List<Formula> arguments){
        this.arguments = new Formula[arguments.size()];

        for(int i = 0; i< arguments.size(); i++){
            this.arguments[i] = arguments.get(i);
        }

        this.subFormulae = Arrays.stream(this.arguments).map(Formula::subFormulae).
                reduce(Sets.newSet(), Sets::union);

        this.variables = arguments.stream().map(Formula::variablesPresent).reduce(Sets.newSet(), Sets::union);

    }
    public Formula[] getArguments() {
        return arguments.clone();
    }

    @Override
    public String toString() {
        return "(and "  + Common.toString(arguments) + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        And and = (And) o;

        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return Arrays.equals(arguments, and.arguments);

    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(arguments);
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
        return new And(Arrays.stream(arguments).map(x->x.apply(substitution)).collect(Collectors.toList()));

    }

    @Override
    public Formula shadow(int level) {
        return new And(Arrays.stream(arguments).map(f->shadow(level)).collect(Collectors.toList()));
    }

    @Override
    public Formula applyOperation(UnaryOperator<Formula> operator) {
        return new And(Arrays.stream(arguments).map(x->x.applyOperation(operator)).collect(Collectors.toList()));

    }
}
