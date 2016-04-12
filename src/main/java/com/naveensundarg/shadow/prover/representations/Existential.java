package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.representations.Formula;
import com.naveensundarg.shadow.prover.representations.Variable;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Set;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Existential extends Formula {

    private final Formula argument;
    private final Variable[] variables;
    private Set<Formula> subFormulae;

    public Existential(Variable[] variables, Formula argument){

        if(!(variables.length>0)){
            throw new AssertionError("Existential should have at least one variable");
        }

        this.variables = variables;
        this.argument = argument;
        this.subFormulae = Sets.with(argument);
    }


    @Override
    public Set<Formula> subFormulae() {
        return subFormulae;
    }

    @Override
    public String toString() {
        return "\u2203" + "["+ Arrays.toString(variables) + "]"
                + subFormulae;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Existential existential = (Existential) o;

        if (argument != null ? !argument.equals(existential.argument) : existential.argument != null) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(variables, existential.variables)) return false;
        return subFormulae != null ? subFormulae.equals(existential.subFormulae) : existential.subFormulae == null;

    }

    @Override
    public int hashCode() {
        int result = argument != null ? argument.hashCode() : 0;
        result = 31 * result + Arrays.hashCode(variables);
        result = 31 * result + (subFormulae != null ? subFormulae.hashCode() : 0);
        return result;
    }
}
