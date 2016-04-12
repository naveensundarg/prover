package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Common;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class And extends Formula {

    private final Formula[] arguments;

    private final Set<Formula> subFormulae;
    public And(Formula ... arguments){
        this.arguments = arguments;
        this.subFormulae = Arrays.stream(arguments).map(x->x.subFormulae()).
                reduce(Sets.newSet(), (x,y)-> Sets.union(x,y));
    }

    public And(List<Formula> arguments){
        this.arguments = new Formula[arguments.size()];

        for(int i = 0; i< arguments.size(); i++){
            this.arguments[i] = arguments.get(i);
        }

        this.subFormulae = Arrays.stream(this.arguments).map(x->x.subFormulae()).
                reduce(Sets.newSet(), (x,y)-> Sets.union(x,y));
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
}
