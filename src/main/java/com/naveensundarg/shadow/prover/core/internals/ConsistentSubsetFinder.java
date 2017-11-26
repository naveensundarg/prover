package com.naveensundarg.shadow.prover.core.internals;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import javax.swing.text.html.Option;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Created by naveensundarg on 9/8/17.
 */
public class ConsistentSubsetFinder {


    private final Map<Set<Formula>,Boolean> consistencyTable;
    private final Map<Set<Formula>,Boolean> provabilityTable;

    public ConsistentSubsetFinder() {
        provabilityTable = CollectionUtils.newMap();

        consistencyTable = CollectionUtils.newMap();
    }


    public  Optional<Justification> find(Prover prover, Set<Formula> formulas, Formula antecedent, Formula consequent){

        Set<Formula> augmented = Sets.add(formulas, antecedent);
        boolean isConsistent = Logic.isConsistent(formulas, antecedent);
        Optional<Justification> proofOpt = Optional.empty();

        if(isConsistent){

            proofOpt = prover.prove(augmented, consequent);
            if(proofOpt.isPresent()){
               return proofOpt;
            }
            else{
                return Optional.empty();
            }

        }

        Optional<Optional<Justification>> resultOpt =  formulas.
                stream().
                map(x->Sets.remove(formulas,x)).
                map(subset-> find(prover, subset, antecedent, consequent)).
                filter(Optional::isPresent).findAny();

        return resultOpt.isPresent()? resultOpt.get(): Optional.empty();

    }
}
