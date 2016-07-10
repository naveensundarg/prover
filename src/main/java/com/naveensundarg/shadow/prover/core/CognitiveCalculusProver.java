package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.Formula;

import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 4/21/16.
 */
public class CognitiveCalculusProver implements Prover {

    /*
    *
    */
    public CognitiveCalculusProver(){

    }
    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {
        return null;
    }
}
