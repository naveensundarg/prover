package com.naveensundarg.shadow.prover.core.expanders.inductivecalculus;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.AntiUnifier;
import com.naveensundarg.shadow.prover.representations.formula.Exemplar;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public enum AntiUnification implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        List<Exemplar> examples = new ArrayList(CommonUtils.formulaOfType(base, Exemplar.class));

        if(examples.size()<2){
            return;
        }
        for (int i = 0; i < examples.size(); i++) {

            for (int j = i+1; j < examples.size(); j++) {

                Exemplar ex1 = examples.get(i);
                Exemplar ex2 = examples.get(j);
                if (!ex1.equals(ex2)) {

                    Formula antiUnified = AntiUnifier.antiUnify(ex1, ex2);


                    if(antiUnified.getWeight() == ex1.getWeight() && ex1.getWeight() == ex2.getWeight()){
                        base.remove(ex1);
                        base.remove(ex2);
                        antiUnified.setJustificationLabelAndAncestors(this.getClass().getSimpleName(), CollectionUtils.listOf(ex1, ex2));
                        base.add(antiUnified);
                        added.add(antiUnified);
                    }
                }
            }
        }

    }
}
