package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.Set;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Universal extends Formula{

    private final Formula argument;
    private final Variable[] variables;
    private Set<Formula> subFormulae;

    public Universal(Variable[] variables, Formula argument){

        if(!(variables.length>0)){
            throw new AssertionError("Universal should have at least one variable");
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
        return "\u2200" + "["+ Arrays.toString(variables) + "]"
                 + subFormulae;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Universal universal = (Universal) o;

        if (argument != null ? !argument.equals(universal.argument) : universal.argument != null) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(variables, universal.variables)) return false;
        return subFormulae != null ? subFormulae.equals(universal.subFormulae) : universal.subFormulae == null;

    }

    @Override
    public int hashCode() {
        int result = argument != null ? argument.hashCode() : 0;
        result = 31 * result + Arrays.hashCode(variables);
        result = 31 * result + (subFormulae != null ? subFormulae.hashCode() : 0);
        return result;
    }
}
