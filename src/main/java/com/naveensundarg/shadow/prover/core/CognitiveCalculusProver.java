package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.*;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Logic;

import java.text.Normalizer;
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

        return prove(assumptions, formula, CollectionUtils.newEmptySet());
    }


    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> added) {

        FirstOrderResolutionProver folProver = new FirstOrderResolutionProver();

        Set<Formula> base = CollectionUtils.setFrom(assumptions);
        Formula shadowedGoal = formula.shadow(1);

        Optional<Justification> shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);

        while (!shadowedJustificationOpt.isPresent()) {

            int sizeBeforeExpansion = base.size();
            base = expand(base, added);
            int sizeAfterExpansion = base.size();

            Optional<Justification> caseProofOpt = tryOR(base, formula, added);


            if (caseProofOpt.isPresent()) {
                return caseProofOpt;
            }

            Optional<Justification> reductioProofOpt = tryReductio(base, formula, added);


            if (reductioProofOpt.isPresent()) {
                return reductioProofOpt;
            }
            if (sizeAfterExpansion <= sizeBeforeExpansion) {
                return Optional.empty();
            }


            shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);
        }

        return shadowedJustificationOpt;

    }


    private Optional<Justification> tryOR(Set<Formula> base, Formula formula, Set<Formula> added) {

        Set<Or> level2ORs = level2FormulaeOfType(base, Or.class);

        Optional<Or> someOrOpt = level2ORs.stream().findAny();

        if (someOrOpt.isPresent()) {

            Or someOr = someOrOpt.get();
            Formula[] disjuncts = someOr.getArguments();

            Set<Formula> reducedBase = CollectionUtils.setFrom(base);
            reducedBase.remove(someOr);

            List<Optional<Justification>> casesOpt = Arrays.stream(disjuncts).map(disjunct -> {
                CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();

                Set<Formula> newBase = CollectionUtils.setFrom(reducedBase);
                newBase.add(disjunct);

                return cognitiveCalculusProver.prove(newBase, formula, CollectionUtils.setFrom(added));

            }).collect(Collectors.toList());

            boolean proved = casesOpt.stream().allMatch(Optional::isPresent);

            if (proved) {
                return Optional.of(new CompoundJustification("OR", casesOpt.stream().map(Optional::get).collect(Collectors.toList())));
            } else {
                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }

    }

    private Optional<Justification> tryReductio(Set<Formula> base, Formula formula, Set<Formula> added) {

        Formula negated = Logic.negated(formula);
        if (base.contains(negated) || formula.toString().startsWith("$")) {
            return Optional.empty();
        }

        Atom atom = Atom.generate();


        Set<Formula> augmented = CollectionUtils.setFrom(base);

        augmented.add(negated);
        CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();

        Optional<Justification> reductioJustOpt = cognitiveCalculusProver.prove(augmented, atom, added);

        return reductioJustOpt;
    }

    private Set<Formula> expand(Set<Formula> base, Set<Formula> added) {
        breakUpBiConditionals(base);

        expandR4(base, added);
        expandR11a(base, added);
        expandDR6(base, added);
        expandDR6a(base, added);
        expandModalConjunctions(base, added);
        expandModalImplications(base, added);
        expandDR1(base, added);
        expandDR2(base, added);
        expandDR3(base, added);

        return base;
    }

    private void breakUpBiConditionals(Set<Formula> base) {


        Set<BiConditional> biConditionals = formulaOfType(base, BiConditional.class);

        biConditionals.forEach(biConditional -> {
            base.add(new Implication(biConditional.getLeft(), biConditional.getRight()));
            base.add(new Implication(biConditional.getRight(), biConditional.getLeft()));
            base.add(new Implication(Logic.negated(biConditional.getLeft()), Logic.negated(biConditional.getRight())));
            base.add(new Implication(Logic.negated(biConditional.getRight()), Logic.negated(biConditional.getLeft())));

        });

    }

    private void expandR4(Set<Formula> base, Set<Formula> added) {

        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Knowledge).
                map(f -> ((Knowledge) f).getFormula()).
                filter(f -> !added.contains(f)).
                collect(Collectors.toSet());

        base.addAll(derived);
        added.addAll(derived);

    }

    private void expandR11a(Set<Formula> base, Set<Formula> added) {

        Set<Belief> implicationBeliefs =
                level2FormulaeOfTypeWithConstraint(base, Belief.class, b -> ((Belief) b).getFormula() instanceof Implication);


        Set<Formula> validConsequentBeliefss = implicationBeliefs.stream().
                filter(b -> base.contains(new Belief(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getAntecedent()))).
                map(b -> new Belief(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getConsequent())).
                filter(f -> !added.contains(f)).
                collect(Collectors.toSet());

        base.addAll(validConsequentBeliefss);
        added.addAll(added);

    }

    private void expandDR1(Set<Formula> base, Set<Formula> added) {
        Set<Common> commons = level2FormulaeOfType(base, Common.class);
        Set<Value> agents = Logic.allAgents(base);
        List<List<Value>> agent1Agent2 = CommonUtils.setPower(agents, 2);

        for (Common c : commons) {
            for (List<Value> agentPair : agent1Agent2) {
                Formula formula = c.getFormula();
                Value time = c.getTime();
                Knowledge inner = new Knowledge(agentPair.get(1), time, formula);
                Knowledge outer = new Knowledge(agentPair.get(0), time, inner);

                if (!added.contains(outer)) {
                    base.add(outer);
                    added.add(outer);
                }

            }
        }


    }

    private void expandDR2(Set<Formula> base, Set<Formula> added) {
        Set<Common> commons = level2FormulaeOfType(base, Common.class);
        Set<Value> agents = Logic.allAgents(base);

        for (Common c : commons) {
            for (Value agent : agents) {
                Formula formula = c.getFormula();
                Value time = c.getTime();
                Knowledge knowledge = new Knowledge(agent, time, formula);

                if (!added.contains(knowledge)) {
                    base.add(knowledge);
                    added.add(knowledge);
                }
            }
        }


    }

    private void expandDR3(Set<Formula> base, Set<Formula> added) {
        Set<Common> commons = level2FormulaeOfType(base, Common.class);

        for (Common c : commons) {
            Formula formula = c.getFormula();
            if (!added.contains(formula)) {
                base.add(formula);
                added.add(formula);
            }
        }


    }

    private void expandDR6(Set<Formula> base, Set<Formula> added) {

        Set<Knowledge> implicationKnowledge =
                level2FormulaeOfTypeWithConstraint(base, Knowledge.class, b -> ((Knowledge) b).getFormula() instanceof Implication);


        Set<Formula> validConsequentKnowledge = implicationKnowledge.stream().
                filter(b -> base.contains(new Knowledge(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getAntecedent()))).
                map(b -> new Knowledge(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getConsequent())).
                filter(x->!added.contains(x)).
                collect(Collectors.toSet());

        added.addAll(validConsequentKnowledge);
        base.addAll(validConsequentKnowledge);

    }

    private void expandDR6a(Set<Formula> base, Set<Formula> added) {

        Set<Knowledge> biConditionalKnowledge =
                level2FormulaeOfTypeWithConstraint(base, Knowledge.class, b -> ((Knowledge) b).getFormula() instanceof BiConditional);


        Set<Formula> validRights = biConditionalKnowledge.stream().
                filter(b -> base.contains(new Knowledge(b.getAgent(), b.getTime(), ((BiConditional) b.getFormula()).getLeft()))).
                map(b -> new Knowledge(b.getAgent(), b.getTime(), ((BiConditional) b.getFormula()).getRight())).
                filter(x->!added.contains(x)).
                collect(Collectors.toSet());

        added.addAll(validRights);
        base.addAll(validRights);

        Set<Formula> validLefts = biConditionalKnowledge.stream().
                filter(b -> base.contains(new Knowledge(b.getAgent(), b.getTime(), ((BiConditional) b.getFormula()).getRight()))).
                map(b -> new Knowledge(b.getAgent(), b.getTime(), ((BiConditional) b.getFormula()).getLeft())).
                filter(x->!added.contains(x)).
                collect(Collectors.toSet());

        added.addAll(validLefts);
        base.addAll(validLefts);

    }

    private void expandModalConjunctions(Set<Formula> base, Set<Formula> added) {

        Set<And> level2Ands = level2FormulaeOfType(base, And.class);

        for (And and : level2Ands) {

            Set<Formula> level2Conjuncts = Arrays.stream(and.getArguments()).
                    filter(conjunct -> conjunct.getLevel() == 2).
                    filter(x->!added.contains(x)).
                    collect(Collectors.toSet());

            added.addAll(level2Conjuncts);
            base.addAll(level2Conjuncts);


        }
    }

    private void expandModalImplications(Set<Formula> base, Set<Formula> added) {


        Set<Implication> level2Ifs = level2FormulaeOfType(base, Implication.class);

        for (Implication implication : level2Ifs) {

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();

            Set<Formula> reducedBase = CollectionUtils.setFrom(base);

            reducedBase.remove(implication);

            Optional<Justification> antecedentJustificationOpt = cognitiveCalculusProver.prove(reducedBase, antecedent, CollectionUtils.setFrom(added));
            if (antecedentJustificationOpt.isPresent()) {
                if(!added.contains(consequent)){
                    base.add(consequent);
                    added.add(consequent);
                }
            }

            Set<Formula> newReducedBase = CollectionUtils.setFrom(base);
            newReducedBase.remove(implication);
            Optional<Justification> negatedConsequentJustificationOpt = cognitiveCalculusProver.prove(newReducedBase, Logic.negated(consequent), CollectionUtils.setFrom(added));
            if (negatedConsequentJustificationOpt.isPresent()) {
                if(!added.contains(consequent)){
                    base.add(Logic.negated(antecedent));
                    added.add(Logic.negated(antecedent));
                }
            }


        }


    }

    private static <T> Set<T> formulaeOfTypeWithConstraint(Set<Formula> formulas, Class c, java.util.function.Predicate<Formula> constraint) {

        return formulas.
                stream().
                filter(c::isInstance).
                filter(constraint).
                map(f -> (T) f).
                collect(Collectors.toSet());
    }

    private static <T> Set<T> formulaOfType(Set<Formula> formulas, Class c) {

        return formulaeOfTypeWithConstraint(formulas, c, f -> true);

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

        return level2FormulaeOfTypeWithConstraint(formulas, c, f -> true);

    }

    private Set<Formula> shadow(Set<Formula> formulas) {
        return formulas.stream().map(f -> f.shadow(1)).collect(Collectors.toSet());
    }


}
