package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

/**
 * Created by naveensundarg on 4/21/16.
 */
public class HaloCognitiveCalculusProver implements Prover {

    /*
     *
     */



    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        return prove(assumptions, formula, CollectionUtils.newEmptySet());
    }


    private synchronized  Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> added) {



        Prover folProver = SnarkWrapper.getInstance();

        Set<Formula> base =  CollectionUtils.setFrom(assumptions);

        Formula shadowedGoal = formula.shadow(1);

        Optional<Justification> shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);

        Optional<Justification> agentClosureJustificationOpt = this.proveAgentClosure(base, formula);


        PriorityQueue<Formula> weightQueue = new PriorityQueue<>(Comparator.comparing(Formula::getWeight));
        Queue<Formula> ageQueue = new LinkedList<>();

        for (Formula f : assumptions) {
            weightQueue.add(f);
            ageQueue.add(f);
        }

        Set<Formula> usableList = CollectionUtils.newEmptySet();

       // usableList.add(weightQueue.remove());

        int ageWeightCounter  = 0;

        int k = 5;
        while (!weightQueue.isEmpty() && !ageQueue.isEmpty()) {

            Formula given;
            if(ageWeightCounter%k == 0) {

                given = ageQueue.remove();
                weightQueue.remove(given);

            }
            else {
                given = weightQueue.remove();
                ageQueue.remove(given);

            }

            ageWeightCounter = ageWeightCounter + 1;

            int sizeBeforeExpansion = base.size();
            base = expand(base, added, formula);
            int sizeAfterExpansion = base.size();




            if (sizeAfterExpansion <= sizeBeforeExpansion) {
                return Optional.empty();
            }

            shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);
            agentClosureJustificationOpt = proveAgentClosure(base, formula);

            if (shadowedJustificationOpt.isPresent()) {

                return shadowedJustificationOpt;
            }

            if (agentClosureJustificationOpt.isPresent()) {

                return agentClosureJustificationOpt;
            }

        }


        return Optional.empty();
    }

    private Optional<Justification> proveAgentClosure(Set<Formula> base, Formula goal) {

        if (goal instanceof Belief) {

            Belief belief = (Belief) goal;
            Value agent = belief.getAgent();
            Value time = belief.getTime();
            Formula goalBelief = belief.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> allBelievedTillTime = agentSnapShot.allBelievedByAgentTillTime(agent, time);


            HaloCognitiveCalculusProver cognitiveCalculusProver = new HaloCognitiveCalculusProver();
            Optional<Justification> inner = cognitiveCalculusProver.prove(allBelievedTillTime, goalBelief);
            if (inner.isPresent()) {
                //TODO: Augment this

                return inner;
            }

        }

        if (goal instanceof Knowledge) {


            Knowledge knowledge = (Knowledge) goal;
            Value agent = knowledge.getAgent();
            Value time = knowledge.getTime();
            Formula goalKnowledge = knowledge.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> allKnownByTillTime = agentSnapShot.allKnownByAgentTillTime(agent, time);


            HaloCognitiveCalculusProver cognitiveCalculusProver = new HaloCognitiveCalculusProver();
            Optional<Justification> inner = cognitiveCalculusProver.prove(allKnownByTillTime, goalKnowledge);
            if (inner.isPresent()) {
                //TODO: Augment this

                return inner;
            }
        }

        return Optional.empty();

    }


    private Set<Formula> expand(Set<Formula> base, Set<Formula> added, Formula goal) {

        expandR4(base, added);
        expandPerceptionToKnowledge(base, added);

        expandDR1(base, added, goal);
        expandDR2(base, added, goal);
        expandDR3(base, added);
        expandDR5(base, added);

        expandOughtRule(base, added);
        expandUniversalElim(base, added, goal);
        return base;
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

    private void expandPerceptionToKnowledge(Set<Formula> base, Set<Formula> added) {

        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Perception).
                map(f -> {
                    Perception p = (Perception) f;
                    return new Knowledge(p.getAgent(), p.getTime(), p.getFormula());
                }).
                collect(Collectors.toSet());

        base.addAll(derived);
        added.addAll(derived);

    }


    private void expandOughtRule(Set<Formula> base, Set<Formula> added) {

        java.util.function.Predicate<Formula> filterSelfOughts = f -> {

            if (!(f instanceof Belief)) {

                return false;
            }

            Belief b = (Belief) f;
            Formula believed = b.getFormula();
            if (!(believed instanceof Ought)) {
                return false;
            }
            Ought ought = (Ought) believed;
            Formula precondition = ought.getPrecondition();
            Value outerAgent = b.getAgent();
            Value innerAgent = ought.getAgent();
            return innerAgent.equals(outerAgent);
        };

        Set<Belief> obligationBeliefs =
                level2FormulaeOfTypeWithConstraint(base,
                        Belief.class,
                        filterSelfOughts);


        for (Belief b : obligationBeliefs) {
            Ought ought = (Ought) b.getFormula();
            Formula precondition = ought.getPrecondition();
            Value outerTime = b.getTime();
            Value agent = b.getAgent();
            Belief preConditionBelief = new Belief(agent, outerTime, precondition);
            HaloCognitiveCalculusProver cognitiveCalculusProver = new HaloCognitiveCalculusProver();
            Set<Formula> smaller = CollectionUtils.setFrom(base);
            smaller.remove(b);
            smaller = smaller.stream().filter(x -> !x.subFormulae().contains(ought)).collect(Collectors.toSet());
            Optional<Justification> preconditionBelievedOpt = cognitiveCalculusProver.prove(smaller, preConditionBelief);

            if (preconditionBelievedOpt.isPresent()) {
                Formula oughtAction = ought.getOught();
                //  Value action = new Compound("action", new Value[]{agent, actionType});
                //Formula happens = new Predicate("happens", new Value[]{action, ought.getTime()});
                added.add(oughtAction);
                base.add(oughtAction);
            }


        }


    }

    private void expandDR1(Set<Formula> base, Set<Formula> added, Formula goal) {
        Set<Common> commons = level2FormulaeOfType(base, Common.class);
        Set<Value> agents = Logic.allAgents(CollectionUtils.addToSet(base, goal));
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

    private void expandDR5(Set<Formula> base, Set<Formula> added) {
        Set<Knowledge> knows = level2FormulaeOfType(base, Knowledge.class);

        for (Knowledge k : knows) {

            Value agent = k.getAgent();
            Value time = k.getTime();
            Formula formula = k.getFormula();

            Belief belief = new Belief(agent, time, formula);
            if (!added.contains(belief)) {
                base.add(belief);
                added.add(belief);
            }

        }
    }


    private void expandDR2(Set<Formula> base, Set<Formula> added, Formula goal) {
        Set<Common> commons = level2FormulaeOfType(base, Common.class);
        Set<Value> agents = Logic.allAgents(CollectionUtils.addToSet(base, goal));

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

    private void expandUniversalElim(Set<Formula> base, Set<Formula> added, Formula goal) {

        //TODO: Less stupid elimination

        Set<Formula> formulae = CollectionUtils.setFrom(base);
        formulae.add(goal);

        Set<Universal> universals = base.stream().filter(f -> f instanceof Universal).map(f -> (Universal) f).collect(Collectors.toSet());

/*
        Set<Value> values = Logic.baseFormulae(formulae).stream().
                map(Predicate::allValues).
                reduce(Sets.newSet(), Sets::union).stream().filter(v -> !(v instanceof Variable) ).

                collect(Collectors.toSet());
*/


        universals.stream().forEach(universal -> {

            Formula formula = universal.getArgument();

            List<Set<Value>> smartValues = UniversalInstantiation.smartHints(universal, formulae);

            Set<List<Value>> substitutions = cartesianProduct(smartValues);
            Variable[] vars = universal.vars();

            Map<Variable, Value> mapping = CollectionUtils.newMap();
            substitutions.stream().forEach(substitution -> {

                        for (int i = 0; i < vars.length; i++) {

                            mapping.put(vars[i],substitution.get(vars.length-1-i));


                        }

                        Formula derived = universal.getArgument().apply(mapping);

                       if(!added.contains(derived)){
                            base.add(derived);
                            added.add(derived);
                        }

                    }

            );


        });

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
