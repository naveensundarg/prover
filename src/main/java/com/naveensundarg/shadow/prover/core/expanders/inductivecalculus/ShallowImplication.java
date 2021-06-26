package com.naveensundarg.shadow.prover.core.expanders.inductivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.CognitiveCalculusProver;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public enum ShallowImplication implements Expander {


    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        Set<Implication> ifs = CommonUtils.formulaOfType(base, Implication.class);


        for (Implication implication : ifs) {


            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();


            Set<Formula> reducedBase = CollectionUtils.setFrom(base);

            reducedBase.remove(implication);

            if (base.contains(antecedent)) {
                base.add(consequent);

            }



        }

    }



}
