package com.naveensundarg.shadow.prover.axiomsets;

import com.naveensundarg.shadow.prover.constraints.NoFreeVariablesConstraint;
import com.naveensundarg.shadow.prover.constraints.Signature;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Set;

/**
 * Created by naveensundarg on 9/9/17.
 */
public enum DiscreteEventCalculus implements AxiomSet{


    INSTANCE;

    private final Set<Formula> axioms;

    DiscreteEventCalculus(){

        axioms = AxiomSet.readFromFile(DiscreteEventCalculus.class.getResourceAsStream("discrete-event-calculus.clj"));


        NoFreeVariablesConstraint.INSTANCE.satisfies(axioms);

        Signature signature = new Signature(Signature.class.getResourceAsStream("discrete-event-calculus.clj"));
        signature.satisfies(axioms);


    }

    @Override
    public AxiomSet getInstance() {
            return INSTANCE;
    }

    @Override
    public Set<Formula> get() {
        return axioms;
    }
}
