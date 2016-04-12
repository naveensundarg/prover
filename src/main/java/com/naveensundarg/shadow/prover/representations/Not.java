package com.naveensundarg.shadow.prover.representations;

import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Not extends Formula {

    private final Formula argument;

    private final Set<Formula> subFormuale;
    public Not(Formula argument){
        this.argument = argument;
        this.subFormuale = argument.subFormulae();
    }


    public Formula getArgument() {
        return argument;
    }



    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Not not = (Not) o;

        return argument.equals(not.argument);

    }

    @Override
    public int hashCode() {
        return argument.hashCode();
    }

    @Override
    public String toString() {
        return "(not " + argument +")";
    }

    @Override
    public Set<Formula> subFormulae() {
        return subFormuale;
    }
}
