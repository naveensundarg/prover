package com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.InferenceJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.BiConditional;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;
import com.naveensundarg.shadow.prover.representations.formula.Predicate;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;

import java.util.Set;
import java.util.stream.Collectors;

public enum EqualitySubstitution implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {



        Set<Predicate> allEqualPredicates = base.stream()
                .filter(x-> x instanceof Predicate)
                .map(x -> (Predicate) x)
                .filter(x->x.getName().equals("="))
                .collect(Collectors.toSet());


        allEqualPredicates.forEach(equality -> {

            Value left = equality.getArguments()[0];
            Value right = equality.getArguments()[1];
            Justification j = InferenceJustification.from(this.getClass().getSimpleName(), equality);

            base.add(goal.replace(left, right).setJustification(j));
            base.add(goal.replace(right, left).setJustification(j));
        });

    }



}
