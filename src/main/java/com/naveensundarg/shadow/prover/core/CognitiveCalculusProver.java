package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.*;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/21/16.
 */
public class CognitiveCalculusProver implements Prover {

    /*
     *
     */

    public CognitiveCalculusProver() {

    }

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        FirstOrderResolutionProver folProver = new FirstOrderResolutionProver();

        Set<Formula> base = CollectionUtils.setFrom(assumptions);
        Formula shadowedGoal = formula.shadow(1);

        Optional<Justification> shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);
        while (!shadowedJustificationOpt.isPresent()) {

            int sizeBeforeExpansion = base.size();
            base = expand(base);
            int sizeAfterExpansion = base.size();

            Optional<Justification> caseProofOpt  =  tryOR(base, formula);

            
            if(caseProofOpt.isPresent()){
                return caseProofOpt;
            }
            if (sizeAfterExpansion <= sizeBeforeExpansion) {
                return Optional.empty();
            }

            shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);
        }

        return shadowedJustificationOpt;

    }


    private Optional<Justification> tryOR(Set<Formula> base, Formula formula){

        Set<Or> level2ORs = level2FormulaeOfType(base, Or.class);

        Optional<Or> someOrOpt = level2ORs.stream().findAny();

        if(someOrOpt.isPresent()){

            Or someOr = someOrOpt.get();
            Formula[] disjuncts = someOr.getArguments();

            Set<Formula> reducedBase = CollectionUtils.setFrom(base);
            reducedBase.remove(someOr);

            List<Optional<Justification>> casesOpt = Arrays.stream(disjuncts).map(disjunct -> {
               CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();

                Set<Formula> newBase = CollectionUtils.setFrom(reducedBase);
                newBase.add(disjunct);
                return cognitiveCalculusProver.prove(newBase, formula);

            }).collect(Collectors.toList());

            boolean proved = casesOpt.stream().allMatch(Optional::isPresent);

            if(proved){
                return Optional.of(new CompoundJustification("OR",  casesOpt.stream().map(Optional::get).collect(Collectors.toList())));
            }
            else{
                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }

    }


    private Set<Formula> expand(Set<Formula> base) {
        expandR4(base);
        expandR11a(base);
        expandModalConjunctions(base);
        expandModalImplications(base);
        return base;
    }

    private void expandR4(Set<Formula> base) {

        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Knowledge).
                map(f -> ((Knowledge) f).getFormula()).
                filter(f -> !base.contains(f)).
                collect(Collectors.toSet());

        base.addAll(derived);


    }

    private void expandR11a(Set<Formula> base) {

        Set<Belief> implicationBeliefs =
                level2FormulaeOfTypeWithConstraint(base, Belief.class, b -> ((Belief) b).getFormula() instanceof Implication);


        Set<Formula> validConsequentBeliefss = implicationBeliefs.stream().
                filter(b -> base.contains(new Belief(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getAntecedent()))).
                map(b -> new Belief(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getConsequent())).collect(Collectors.toSet());

        base.addAll(validConsequentBeliefss);

    }

    private void expandModalConjunctions(Set<Formula> base) {

        Set<And> level2Ands = level2FormulaeOfType(base, And.class);

        for (And and : level2Ands) {

            Set<Formula> level2Conjuncts = Arrays.stream(and.getArguments()).filter(conjunct -> conjunct.getLevel() == 2).collect(Collectors.toSet());

            base.addAll(level2Conjuncts);

        }
    }

    private void expandModalImplications(Set<Formula> base) {


        Set<Implication> level2Ifs = level2FormulaeOfType(base, Implication.class);

        for (Implication implication : level2Ifs) {

            Formula antecedent = implication.getAntecedent();

            CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();
            Optional<Justification> antecedentJustificationOpt = cognitiveCalculusProver.prove(base, antecedent);
            if (antecedentJustificationOpt.isPresent()) {
                base.add(implication.getConsequent());
            }

        }


    }

    private static <T> Set<T> level2FormulaeOfTypeWithConstraint(Set<Formula> formulas, Class c, java.util.function.Predicate<Formula> constraint) {

        return formulas.
                stream().
                filter(a -> a.getLevel() == 2).
                filter(c::isInstance).
                filter(constraint).
                map(f -> (T) f).
                collect(Collectors.toSet());
    }

    private static <T> Set<T> level2FormulaeOfType(Set<Formula> formulas, Class c) {

        return level2FormulaeOfTypeWithConstraint(formulas, c, f->true);

    }

    private Set<Formula> shadow(Set<Formula> formulas) {
        return formulas.stream().map(f -> f.shadow(1)).collect(Collectors.toSet());
    }


}
