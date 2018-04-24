package com.naveensundarg.shadow.prover.generators;


import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Problem;

import java.util.List;
import java.util.Set;

public interface Generator {

    List<Pair<List<Formula>, Boolean>> generate(int total);

}
