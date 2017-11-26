package com.naveensundarg.shadow.prover.axiomsets;

import com.naveensundarg.shadow.prover.constraints.NoFreeVariablesConstraint;
import com.naveensundarg.shadow.prover.constraints.Signature;
import com.naveensundarg.shadow.prover.representations.formula.Common;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 9/9/17.
 */
public enum CognitiveTelephone implements AxiomSet {

    INSTANCE;

    private final Set<Formula> axioms;

    CognitiveTelephone(){

        axioms = AxiomSet.readFromFile(CognitiveTelephone.class.getResourceAsStream("telephone.clj")).stream().map(f->new Common(Reader.NOW, f)).collect(Collectors.toSet());

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
