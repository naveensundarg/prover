package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.rules.*;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

/**
 * Created by naveensundarg on 1/1/17.
 */
public class FirstOrderColorHalo extends HaloCore {



    static double weightDelta = 0.5;
    static double weight  = 4;

    private final Set<ForwardClauseRule> rules;


    public FirstOrderColorHalo(Set<ForwardClauseRule> rules) {

        this.rules = rules;
    }

    public FirstOrderColorHalo() {

        this.rules = Sets.newSet();

        rules.add(FirstOrderResolutionImplementation.INSTANCE);
        rules.add(ParamodulationImplementation.INSTANCE);
        rules.add(DemodulationImplementation.INSTANCE);


    }

    @Override
    public Set<ForwardClauseRule> getForwardClauseRules() {
        return rules;
    }

    @Override
    public int getAgeWeightRatio() {
        return 5;
    }

    @Override
    public int getMaxWeight() {
        return 100;
    }

}
