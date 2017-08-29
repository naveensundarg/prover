package com.naveensundarg.shadow.prover.representations.deduction;

import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/27/17.
 */
public abstract  class PrimitiveDeduction extends Deduction {


    public abstract Formula compute(Set<Formula> assumptionBase, List<Phrase> arguments);


    public final Formula compute(Set<Formula> assumptionBase, Phrase ... arguments){

        List<Phrase> args = CollectionUtils.newEmptyList();

        Arrays.stream(arguments).forEach(args::add);

        return compute(assumptionBase, args);
    }


}
