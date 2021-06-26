package com.naveensundarg.shadow.prover.core.expanders.inductivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.AntiUnifier;
import com.naveensundarg.shadow.prover.representations.formula.Exemplar;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;

import java.util.Set;

public enum AntiUnification implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        Set<Exemplar> examples = CommonUtils.formulaOfType(base, Exemplar.class);

        for (Exemplar ex1 : examples) {

            for (Exemplar ex2 : examples) {

                if (!ex1.equals(ex2)) {

                    Formula antiUnified = AntiUnifier.antiUnify(ex1, ex2);
                    base.add(antiUnified);
                    added.add(antiUnified);

                    if(antiUnified.getWeight() == ex1.getWeight() && ex1.getWeight() == ex2.getWeight()){
                        base.remove(ex1);
                        base.remove(ex2);
                    }
                }
            }
        }

    }
}
