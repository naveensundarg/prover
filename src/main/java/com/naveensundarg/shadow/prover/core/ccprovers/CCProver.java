package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Set;

public interface CCProver  extends Prover {
    Set<Formula> getProhibited();
}
