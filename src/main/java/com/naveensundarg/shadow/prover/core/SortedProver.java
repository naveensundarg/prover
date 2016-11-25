package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.sortsystem.SortSystem;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 11/24/16.
 */
public interface SortedProver {

    Optional<Justification> prove(SortSystem sortSystem, Set<Formula> assumptions, Formula formula);

}
