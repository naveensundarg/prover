package com.naveensundarg.shadow.prover.axiomsets;

import com.naveensundarg.shadow.prover.constraints.NoFreeVariablesConstraint;
import com.naveensundarg.shadow.prover.constraints.Signature;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Set;

/**
 * Created by naveensundarg on 9/9/17.
 */
public enum Telephone implements AxiomSet {

    INSTANCE;

    private final Set<Formula> axioms;

    Telephone(){

        axioms = AxiomSet.readFromFile(Telephone.class.getResourceAsStream("telephone.clj"));

        NoFreeVariablesConstraint.INSTANCE.satisfies(axioms);
        Signature signature = new Signature(Signature.class.getResourceAsStream("telephone.clj"));
        signature.satisfies(axioms);

        if(axioms.size() != 33){
            throw new AssertionError("Unexpected number of axioms: " + axioms.size());
        }

        axioms.addAll(DiscreteEventCalculus.INSTANCE.get());

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
