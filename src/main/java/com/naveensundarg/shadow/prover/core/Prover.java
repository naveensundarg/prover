package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.sortsystem.SortSystem;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import org.apache.commons.lang3.NotImplementedException;

import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public interface Prover {

    Optional<Justification> prove(Set<Formula> assumptions, Formula formula);


    default Optional<Justification> prove(SortSystem sortSystem, Set<Formula> assumptions, Formula formula){

        throw new NotImplementedException("Sorted proof system not implemented in: " + this.getClass());

    }


}
