package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Atom extends Formula{

    private final String name;
    private final Set<Formula> subFormulae;


    public  Atom(String name){

        this.name = name;

        this.subFormulae = Sets.newSet();
        subFormulae.add(this);
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Atom atom = (Atom) o;

        return name != null ? name.equals(atom.name) : atom.name == null;

    }

    @Override
    public int hashCode() {
        return name != null ? name.hashCode() : 0;
    }

    @Override
    public Set<Formula> subFormulae() {
        return subFormulae;
    }
}
