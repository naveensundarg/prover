package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.rules.DemodulationImplementation;
import com.naveensundarg.shadow.prover.core.rules.FirstOrderResolutionImplementation;
import com.naveensundarg.shadow.prover.core.rules.ForwardClauseRule;
import com.naveensundarg.shadow.prover.core.rules.ParamodulationImplementation;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Map;
import java.util.Set;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;

/**
 * Created by naveensundarg on 1/1/17.
 */
public class FirstOrderHalo extends HaloCore {


    private Prover propositionalProver;

    static double weightDelta = 0.5;
    static double weight  = 4;

    private final Set<ForwardClauseRule> rules;


    public FirstOrderHalo(Set<ForwardClauseRule> rules) {

        this.rules = rules;
        this.propositionalProver = new PropositionalResolutionProver();
    }

    public FirstOrderHalo() {

        this.rules = Sets.newSet();

        rules.add(FirstOrderResolutionImplementation.INSTANCE);
        rules.add(ParamodulationImplementation.INSTANCE);
        //rules.add(DemodulationImplementation.INSTANCE);

        this.propositionalProver = new PropositionalResolutionProver();

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
        return 15;
    }

}
