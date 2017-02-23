package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Logic;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.time.Duration;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

/**
 * Created by naveensundarg on 4/21/16.
 */
public class CognitiveCalculusProver implements Prover {

    /*
     *
     */

    static Set<Problem> problems = Sets.newSet();
    static boolean defeasible = false;
    private final boolean reductio;
    private final CognitiveCalculusProver parent;
    Set<Formula> currentAssumptions;
    public CognitiveCalculusProver() {

        prohibited  = Sets.newSet();
        parent = null;
        reductio = false;
    }

    private Problem currentProblem;

    private CognitiveCalculusProver(CognitiveCalculusProver parent) {

        prohibited  = CollectionUtils.setFrom(parent.prohibited);
        this.parent = parent;
        reductio = false;
    }

    private CognitiveCalculusProver(CognitiveCalculusProver parent, boolean reductio) {

        prohibited  = CollectionUtils.setFrom(parent.prohibited);
        this.parent = parent;
        this.reductio = reductio;
    }

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        return prove(assumptions, formula, CollectionUtils.newEmptySet());
    }

    Set<Formula> prohibited ;

    private boolean alreadySeen(Set<Formula> assumptions, Formula formula) {


        if(parent == null) {
            return false;
        }


        CognitiveCalculusProver node = parent;
        while(node!=null) {

            if(node.currentProblem.equals(currentProblem)){

                return true;
            }

            node = node.parent;

        }

        return false;

    }

    private synchronized  Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> added) {



        currentAssumptions = assumptions;



        if(defeasible){
            Optional<Justification> got = defeasible(formula);
        if (got != null) return got;

        }


        Prover folProver = new SnarkWrapper();

        Set<Formula> base =  CollectionUtils.setFrom(assumptions);

        Formula shadowedGoal = formula.shadow(1);

        Optional<Justification> shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);

        Optional<Justification> agentClosureJustificationOpt = this.proveAgentClosure(base, formula);

        while (!shadowedJustificationOpt.isPresent() && !agentClosureJustificationOpt.isPresent()) {

            int sizeBeforeExpansion = base.size();
            base = expand(base, added, formula);
            int sizeAfterExpansion = base.size();

            if(base.contains(formula)){
                return Optional.of(TrivialJustification.trivial(formula));
            }

            Optional<Justification> andProofOpt = tryAND(base, formula, added);


            if (andProofOpt.isPresent()) {
                return andProofOpt;
            }


            Optional<Justification> caseProofOpt = tryOR(base, formula, added);




            if (caseProofOpt.isPresent()) {
                return caseProofOpt;
            }
            if(base.size()<20 && !reductio) {

                Optional<Justification> reductioProofOpt = tryReductio(base, formula, added);


                if (reductioProofOpt.isPresent()) {
                    return reductioProofOpt;
                }
            }


            if (sizeAfterExpansion <= sizeBeforeExpansion) {
                return Optional.empty();
            }

            shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);
            agentClosureJustificationOpt = proveAgentClosure(base, formula);
        }

        if (shadowedJustificationOpt.isPresent()) {

            return shadowedJustificationOpt;
        }

        if (agentClosureJustificationOpt.isPresent()) {

            return agentClosureJustificationOpt;
        }

        return Optional.empty();
    }

    private Optional<Justification> defeasible(Formula formula) {
        if(formula instanceof CanProve){

            CognitiveCalculusProver outer = this;

            final Duration timeout = Duration.ofSeconds(10);
            ExecutorService executor = Executors.newSingleThreadExecutor();

            final Future<Optional<Justification>> handler = executor.submit((Callable) () -> {

                CognitiveCalculusProver root  = root(outer);

                CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();

                return cognitiveCalculusProver.prove(root.currentAssumptions.stream().filter(x->!x.subFormulae().contains(formula)).collect(Collectors.toSet()),
                        ((CanProve) formula).getFormula());
            });

            try {
                Optional<Justification> got =  handler.get(timeout.toMillis(), TimeUnit.MILLISECONDS);

                return got;

            } catch (TimeoutException e) {
                handler.cancel(true);
                return Optional.empty();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } catch (ExecutionException e) {
                e.printStackTrace();
            }

            executor.shutdownNow();

        }

        if(formula instanceof Not){

            Formula argument = ((Not) formula).getArgument();
            if(argument instanceof CanProve){

                CognitiveCalculusProver outer = this;

                final Duration timeout = Duration.ofSeconds(5);
                ExecutorService executor = Executors.newSingleThreadExecutor();

                final Future<Optional<Justification>> handler = executor.submit((Callable) () -> {

                    CognitiveCalculusProver root  = root(outer);

                    CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver();

                    return cognitiveCalculusProver.prove(root.currentAssumptions.stream().filter(x->!x.subFormulae().contains(argument)).collect(Collectors.toSet()),
                            (((CanProve) argument).getFormula()));
                });

                try {
                    Optional<Justification> got =  handler.get(timeout.toMillis(), TimeUnit.MILLISECONDS);
                    executor.shutdownNow();

                    return Optional.empty();


                } catch (TimeoutException | InterruptedException | ExecutionException e) {
                    handler.cancel(true);
                    executor.shutdownNow();

                    return Optional.of(TrivialJustification.trivial(formula));
                }

            }

        }
        return null;
    }


    private Optional<Justification> tryAND(Set<Formula> base, Formula formula, Set<Formula> added) {

        if (formula instanceof And) {

            And and = (And) formula;

            Formula conjuncts[] = and.getArguments();

            List<Optional<Justification>> conjunctProofsOpt = Arrays.stream(conjuncts).map(conjunct -> {

                CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);
                return cognitiveCalculusProver.prove(base, conjunct);
            }).collect(Collectors.toList());


            if (conjunctProofsOpt.stream().allMatch(Optional::isPresent)) {

                return Optional.of(
                        new CompoundJustification("and",
                                conjunctProofsOpt.stream().map(Optional::get).collect(Collectors.toList())));
            }
        }
        return Optional.empty();
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
                CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);

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
        CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this, true);

        Optional<Justification> reductioJustOpt = cognitiveCalculusProver.prove(augmented, atom, added);

        return reductioJustOpt;
    }

    private Optional<Justification> proveAgentClosure(Set<Formula> base, Formula goal) {

        if (goal instanceof Belief) {

            Belief belief = (Belief) goal;
            Value agent = belief.getAgent();
            Value time = belief.getTime();
            Formula goalBelief = belief.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> allBelievedTillTime = agentSnapShot.allBelievedByAgentTillTime(agent, time);


            CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);
            Optional<Justification> inner = cognitiveCalculusProver.prove(allBelievedTillTime, goalBelief);
            if (inner.isPresent()) {
                //TODO: Augment this

                return inner;
            }

        }

        if (goal instanceof Intends) {


            Intends intends = (Intends) goal;
            Value agent = intends.getAgent();
            Value time = intends.getTime();
            Formula goalKnowledge = intends.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> allIntendedByTillTime = agentSnapShot.allIntendedByAgentTillTime(agent, time);


            CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);
            Optional<Justification> inner = cognitiveCalculusProver.prove(allIntendedByTillTime, goalKnowledge);
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


            CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);
            Optional<Justification> inner = cognitiveCalculusProver.prove(allKnownByTillTime, goalKnowledge);
            if (inner.isPresent()) {
                //TODO: Augment this

                return inner;
            }
        }

        return Optional.empty();

    }


    private Set<Formula> expand(Set<Formula> base, Set<Formula> added, Formula goal) {
        breakUpBiConditionals(base);

        expandR4(base, added);
        expandPerceptionToKnowledge(base, added);
        expandModalConjunctions(base, added);
        expandModalImplications(base, added);
        expandDR1(base, added, goal);
        expandDR2(base, added, goal);
        expandDR3(base, added);
        expandDR5(base, added);
        expandOughtRule(base, added);
        expandUniversalElim(base, added, goal);
        if(prohibited!=null) base.removeAll(prohibited);
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
            if (!innerAgent.equals(outerAgent)) {
                return false;
            }


            return true;
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
            CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);
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

                base.add(new Intends(agent, outerTime, oughtAction));
                added.add(new Intends(agent, outerTime, oughtAction));
            }


        }


    }

    private void expandDR1(Set<Formula> base, Set<Formula> added, Formula goal) {
        Set<Common> commons = level2FormulaeOfType(base, Common.class);
        Set<Value> agents = Logic.allAgents(CollectionUtils.addToSet(base, goal));
        ;
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


    private void expandModalConjunctions(Set<Formula> base, Set<Formula> added) {

        Set<And> level2Ands = level2FormulaeOfType(base, And.class);

        for (And and : level2Ands) {

            Set<Formula> level2Conjuncts = Arrays.stream(and.getArguments()).
                    filter(conjunct -> conjunct.getLevel() == 2).
                    filter(x -> !added.contains(x)).
                    collect(Collectors.toSet());

            added.addAll(level2Conjuncts);
            base.addAll(level2Conjuncts);


        }
    }

    private void expandModalImplications(Set<Formula> base, Set<Formula> added) {


        Set<Implication> level2Ifs = level2FormulaeOfType(base, Implication.class);

        for (Implication implication : level2Ifs) {

            if(prohibited.contains(implication)) {
                continue;
            }

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);
            cognitiveCalculusProver.prohibited.addAll(prohibited);
            cognitiveCalculusProver.prohibited.add(implication);

            Set<Formula> reducedBase = CollectionUtils.setFrom(base);

            reducedBase.remove(implication);

            //TODO: use actual ancestors

            Optional<Justification> antecedentJustificationOpt = cognitiveCalculusProver.prove(reducedBase, antecedent, CollectionUtils.setFrom(added));
            if (antecedentJustificationOpt.isPresent()) {
                if (!added.contains(consequent)) {
                    base.add(consequent);
                    added.add(consequent);
                }
            }

            Set<Formula> newReducedBase = CollectionUtils.setFrom(base);
            newReducedBase.remove(implication);


            Optional<Justification> negatedConsequentJustificationOpt = cognitiveCalculusProver.prove(newReducedBase, Logic.negated(consequent), CollectionUtils.setFrom(added));
            if (negatedConsequentJustificationOpt.isPresent()) {
                if (!added.contains(consequent)) {
                    base.add(Logic.negated(antecedent));
                    added.add(Logic.negated(antecedent));
                }
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

    private static CognitiveCalculusProver root(CognitiveCalculusProver cognitiveCalculusProver){


        CognitiveCalculusProver current = cognitiveCalculusProver.parent;

        if(current == null){
            return cognitiveCalculusProver;
        }
        while(current.parent!=null){

            current = current.parent;
        }

        return current;

    }

}
