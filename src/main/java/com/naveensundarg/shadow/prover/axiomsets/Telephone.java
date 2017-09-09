package com.naveensundarg.shadow.prover.axiomsets;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Set;

/**
 * Created by naveensundarg on 9/9/17.
 */
public enum Telephone implements AxiomSet {

    INSTANCE;

    private final Set<Formula> axioms;

    Telephone(){

        axioms = AxiomSet.readFromFile(SimpleEventCalculus.class.getResourceAsStream("simple-event-calculus.clj"));
    }
    @Override
    public AxiomSet getInstance() {
        return null;
    }

    @Override
    public Set<Formula> get() {
        return null;
    }
}
