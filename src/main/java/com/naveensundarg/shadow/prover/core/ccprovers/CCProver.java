package com.naveensundarg.shadow.prover.core.ccprovers;

import com.diogonunes.jcdp.color.ColoredPrinter;
import com.diogonunes.jcdp.color.api.Ansi;
import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.core.internals.ConsistentSubsetFinder;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Constants;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;


public class CCProver implements Prover {

    /*
     *
     */
    private static boolean verbose = true;
    private final boolean reductio;
    private final boolean theoremsToNec = false;
    private final Set<Expander> expanders;
    private Set<Formula> prohibited;
    private Problem currentProblem;



    private static  String indent = "";

    private static ColoredPrinter coloredPrinter = new ColoredPrinter.Builder(1, false)
            .foreground(Ansi.FColor.WHITE).background(Ansi.BColor.BLACK)   //setting format
            .build();

    public CCProver() {

        prohibited = Sets.newSet();
        reductio = false;
        expanders = CollectionUtils.newEmptySet();
    }

    private CCProver(CCProver parent) {

        prohibited = CollectionUtils.setFrom(parent.prohibited);
        reductio = false;
        expanders = CollectionUtils.newEmptySet();
    }

    private CCProver(CCProver parent, boolean reductio) {

        prohibited = CollectionUtils.setFrom(parent.prohibited);
        this.reductio = reductio;
        expanders = CollectionUtils.newEmptySet();
    }

    private static void log(String message) {

        if (verbose) {
            System.out.println(message);

        }
    }



    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        return prove(assumptions, formula, CollectionUtils.newEmptySet());
    }


    private synchronized Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> added) {


        Prover folProver = SnarkWrapper.getInstance();

        Set<Formula> base = CollectionUtils.setFrom(assumptions);

        Formula shadowedGoal = formula.shadow(1);

        Optional<Justification> shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);

        indent = indent + "\t";
        tryAgentClosure(formula);
        indent = indent.substring(0, indent.length()-1);
        Optional<Justification> agentClosureJustificationOpt = this.proveAgentClosure(base, formula);

        while (!shadowedJustificationOpt.isPresent() && !agentClosureJustificationOpt.isPresent()) {

            int sizeBeforeExpansion = base.size();
            base = expand(base, added, formula);
            int sizeAfterExpansion = base.size();

            if (base.contains(formula)) {
                return Optional.of(TrivialJustification.trivial(assumptions, formula));
            }

            Optional<Justification> andProofOpt = tryAND(base, formula, added);


            if (andProofOpt.isPresent()) {
                return andProofOpt;
            }

            if (!reductio) {

                Optional<Justification> necProofOpt = tryNEC(base, formula, added);

                if (necProofOpt.isPresent()) {
                    return necProofOpt;
                }

                Optional<Justification> posProofOpt = tryPOS(base, formula, added);

                if (posProofOpt.isPresent()) {
                    return posProofOpt;
                }

                Optional<Justification> forAllIntroOpt = tryForAllIntro(base, formula, added);

                if (forAllIntroOpt.isPresent()) {
                    return forAllIntroOpt;
                }

                Optional<Justification> existsIntroOpt = tryExistsIntro(base, formula, added);

                if (existsIntroOpt.isPresent()) {
                    return existsIntroOpt;
                }


                Optional<Justification> ifIntroOpt = tryIfIntro(base, formula, added);

                if (ifIntroOpt.isPresent()) {
                    return ifIntroOpt;
                }


                Optional<Justification> counterFacIntroOpt = tryCounterFactIntro(base, formula, added);

                if (counterFacIntroOpt.isPresent()) {
                    return counterFacIntroOpt;
                }


            }


            Optional<Justification> caseProofOpt = tryOR(base, formula, added);


            if (caseProofOpt.isPresent()) {
                return caseProofOpt;
            }
            if (base.size() < 50 && !reductio) {

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

          //  logFOLCall(true, shadow(base), shadowedGoal);
            return shadowedJustificationOpt;
        }
        else{

         //   logFOLCall(false, shadow(base), shadowedGoal);

        }
        if (agentClosureJustificationOpt.isPresent()) {

            return agentClosureJustificationOpt;
        }

        return Optional.empty();
    }

    private Optional<Justification> tryIfIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Implication) {

            Implication implication = (Implication) formula;

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            indent = indent + "\t";
            tryLog("Tying if intro", formula);
            Optional<Justification> consOpt = this.prove(Sets.add(base, antecedent), consequent);
            indent = indent.substring(0, indent.length()-1);

            if (consOpt.isPresent()) {

                return Optional.of(new CompoundJustification("If Intro", CollectionUtils.listOf(consOpt.get())));

            } else {

                return Optional.empty();
            }
            //TODO: the reverse

        } else {

            return Optional.empty();
        }
    }
    private Optional<Justification> tryCounterFactIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof CounterFactual) {

            CounterFactual counterFactual = (CounterFactual) formula;

            Formula antecedent = counterFactual.getAntecedent();
            Formula consequent = counterFactual.getConsequent();

            indent = indent + "\t";
            tryLog("Tying counterfactual intro", formula);
            Optional<Justification> counterfactualIntroOpt = (new ConsistentSubsetFinder()).find(this, base, antecedent, consequent);
            indent = indent.substring(0, indent.length()-1);

            if (counterfactualIntroOpt.isPresent()) {

                return Optional.of(new CompoundJustification("Counterfactual Intro", CollectionUtils.listOf(counterfactualIntroOpt.get())));

            } else {

                return Optional.empty();
            }
            //TODO: the reverse

        } else {

            return Optional.empty();
        }
    }
    private Optional<Justification> tryExistsIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Existential) {

            Existential existential = (Existential) formula;
            Variable[] vars = existential.vars();

            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());

                tryLog("Trying to prove existential", formula);
                return this.prove(base, ((Existential) formula).getArgument().apply(subs));

            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }


        } else {

            return Optional.empty();
        }
    }

    private Optional<Justification> tryForAllIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Universal) {

            Universal universal = (Universal) formula;
            Variable[] vars = universal.vars();

            tryLog("Trying to prove universal" , universal);
            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());
                //TODO: Verify this.
                Optional<Justification> ansOpt = this.prove(base, ((Universal) formula).getArgument().apply(subs));

                if(ansOpt.isPresent()){

                    return Optional.of(new CompoundJustification("ForAllIntro", CollectionUtils.listOf(ansOpt.get())));
                }

                else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }
            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }


        } else if (formula instanceof Not && ((Not) formula).getArgument() instanceof Existential) {

            //formula = (not (exists [vars] kernel)) == (forall [vars] (not kernel))
            Formula kernel = ((Existential) ((Not) formula).getArgument()).getArgument();
            Variable[] vars = ((Existential) ((Not) formula).getArgument()).vars();


            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());
                            tryLog("Trying to prove " , (new Not(kernel)).apply(subs));

                return this.prove(base, (new Not(kernel)).apply(subs));

            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }
        } else {

            return Optional.empty();
        }
    }

    private Optional<Justification> tryNEC(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Necessity) {

            tryLog("Trying to prove necessity", formula);
            Optional<Justification> innerProof = this.prove(Sets.newSet(), ((Necessity) formula).getFormula());

            if (innerProof.isPresent()) {

                return Optional.of(new CompoundJustification("Nec Intro", CollectionUtils.listOf(innerProof.get())));
            } else {

                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }
    }

    private Optional<Justification> tryPOS(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Not && ((Not) formula).getArgument() instanceof Possibility) {

            Formula core = ((Possibility) ((Not) formula).getArgument()).getFormula();

            tryLog("Trying to prove necessity", new Necessity(new Not(core)));

            Optional<Justification> innerProof = this.prove(Sets.newSet(), new Necessity(new Not(core)));

            if (innerProof.isPresent()) {

                return Optional.of(new CompoundJustification("Pos Intro", CollectionUtils.listOf(innerProof.get())));
            } else {

                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }
    }


    private Optional<Justification> tryAND(Set<Formula> base, Formula formula, Set<Formula> added) {

        if (formula instanceof And) {

            And and = (And) formula;
            tryLog("Trying to prove conjunction", and);


            Formula conjuncts[] = and.getArguments();

            List<Optional<Justification>> conjunctProofsOpt = Arrays.stream(conjuncts).map(conjunct -> {

                CCProver cognitiveCalculusProver = new CCProver(this);
                return cognitiveCalculusProver.prove(base, conjunct);
            }).collect(Collectors.toList());


            if (conjunctProofsOpt.stream().allMatch(Optional::isPresent)) {

                return Optional.of(
                        new CompoundJustification("andIntro",
                                conjunctProofsOpt.stream().map(Optional::get).collect(Collectors.toList())));
            }
        }
        return Optional.empty();
    }

    private Optional<Justification> tryOR(Set<Formula> base, Formula formula, Set<Formula> added) {

        Set<Or> level2ORs = CommonUtils.level2FormulaeOfType(base, Or.class);

        Optional<Or> someOrOpt = level2ORs.stream().findAny();

        if (someOrOpt.isPresent()) {

            Or someOr = someOrOpt.get();
            Formula[] disjuncts = someOr.getArguments();

            Set<Formula> reducedBase = CollectionUtils.setFrom(base);
            reducedBase.remove(someOr);

            List<Optional<Justification>> casesOpt = Arrays.stream(disjuncts).map(disjunct -> {
                CCProver cognitiveCalculusProver = new CCProver(this);

                Set<Formula> newBase = CollectionUtils.setFrom(reducedBase);
                newBase.add(disjunct);

                return cognitiveCalculusProver.prove(newBase, formula, CollectionUtils.setFrom(added));

            }).collect(Collectors.toList());

            boolean proved = casesOpt.stream().allMatch(Optional::isPresent);

            if (proved) {
                return Optional.of(new CompoundJustification("ORIntro", casesOpt.stream().map(Optional::get).collect(Collectors.toList())));
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

        tryLog("Reductio on", negated);


        Set<Formula> augmented = CollectionUtils.setFrom(base);

        augmented.add(negated);
        indent = indent + "\t";
        CCProver cognitiveCalculusProver = new CCProver(this, true);
        Optional<Justification> reductioJustOpt = cognitiveCalculusProver.prove(augmented, atom, added);
        indent = indent.substring(0,indent.length()-1);

        return reductioJustOpt.isPresent() ? Optional.of(new CompoundJustification("Reductio", CollectionUtils.listOf(reductioJustOpt.get()))) :
                Optional.empty();
    }

    private Optional<Justification> proveAgentClosure(Set<Formula> base, Formula goal) {

        if (goal instanceof Belief) {

            Belief belief = (Belief) goal;
            Value agent = belief.getAgent();
            Value time = belief.getTime();
            Formula goalBelief = belief.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> allBelievedTillTime = agentSnapShot.allBelievedByAgentTillTime(agent, time);


            CCProver cognitiveCalculusProver = new CCProver(this);
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


            CCProver cognitiveCalculusProver = new CCProver(this);
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


            CCProver cognitiveCalculusProver = new CCProver(this);
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
        expandKnowledgeConjunctions(base, added);
        expandNotExistsToForallNot(base, added);
        if(theoremsToNec){
           expandTheoremsToNecessity(base, added, goal);
        }
        expandNecToPos(base, added, goal);



        if (prohibited != null) base.removeAll(prohibited);
        return base;
    }

    private void expandNecToPos(Set<Formula> base, Set<Formula> added, Formula goal) {

        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Necessity).
                map(f -> (Necessity) f).
                filter(necessity -> necessity.getFormula() instanceof Not).
                map(necessity -> new Not(new Possibility(((Not) necessity.getFormula()).getArgument()))).collect(Collectors.toSet());

        expansionLog(" Necessarily not => Impossible", derived);
        base.addAll(derived);
        added.addAll(derived);
    }

    private void expandNotExistsToForallNot(Set<Formula> base, Set<Formula> added) {

        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Not).
                map(f -> (Not) f).
                filter(not -> not.getArgument() instanceof Existential).
                map(notExists -> {
                    Existential existential = (Existential) notExists.getArgument();
                    Variable[] variables = existential.vars();
                    Formula kernel = existential.getArgument();
                    return new Universal(variables, new Not(kernel));
                }).collect(Collectors.toSet());

        expansionLog("Not exists => Forall not", derived);

        base.addAll(derived);
        added.addAll(derived);
    }

    private void expandKnowledgeConjunctions(Set<Formula> base, Set<Formula> added) {

        Set<Formula> derived = base.
                stream().
                filter(f -> f instanceof Knowledge).
                map(f -> (Knowledge) f).
                filter(k -> k.getFormula() instanceof And).
                flatMap(k -> {

                    Value agent = k.getAgent();
                    Value time = k.getTime();
                    And and = (And) k.getFormula();

                    return Arrays.stream(and.getArguments()).map(conjunct -> new Knowledge(agent, time, conjunct));

                }).collect(Collectors.toSet());

        expansionLog("Know(P and Q) ==> Know(P) and Know(Q)", derived);

        base.addAll(derived);
        added.addAll(derived);
    }

    private void expandTheoremsToNecessity(Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<Formula> theorems = base.stream().map(Formula::subFormulae).reduce(Sets.newSet(), Sets::union).
                stream().filter(x -> !x.equals(goal) && this.prove(Sets.newSet(), x).isPresent()).collect(Collectors.toSet());

        Set<Formula> necs = theorems.stream().map(Necessity::new).collect(Collectors.toSet());

        expansionLog(String.format("{} %s %s ==>  %s %s", Constants.VDASH, Constants.PHI, Constants.NEC, Constants.PHI), necs);

        base.addAll(necs);
        added.addAll(necs);

    }

    private void breakUpBiConditionals(Set<Formula> base) {


        Set<BiConditional> biConditionals = CommonUtils.formulaOfType(base, BiConditional.class);

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

        if(!base.containsAll(derived)){
                 expansionLog(String.format("Knows(P) ==> P", Constants.VDASH, Constants.PHI, Constants.NEC, Constants.PHI), derived);

        }

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

        expansionLog(String.format("Perceives(P) ==> P", Constants.VDASH, Constants.PHI, Constants.NEC, Constants.PHI), derived);

        base.addAll(derived);
        added.addAll(derived);

    }

    private void expandR11a(Set<Formula> base, Set<Formula> added) {

        Set<Belief> implicationBeliefs =
                CommonUtils.level2FormulaeOfTypeWithConstraint(base, Belief.class, b -> ((Belief) b).getFormula() instanceof Implication);


        Set<Formula> validConsequentBeliefss = implicationBeliefs.stream().
                filter(b -> base.contains(new Belief(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getAntecedent()))).
                map(b -> new Belief(b.getAgent(), b.getTime(), ((Implication) b.getFormula()).getConsequent())).
                filter(f -> !added.contains(f)).
                collect(Collectors.toSet());

        base.addAll(validConsequentBeliefss);
        added.addAll(added);

    }

    private void expandOughtRule(Set<Formula> base, Set<Formula> added) {

        Predicate<Formula> filterSelfOughts = f -> {

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
                CommonUtils.level2FormulaeOfTypeWithConstraint(base,
                        Belief.class,
                        filterSelfOughts);


        for (Belief b : obligationBeliefs) {
            Ought ought = (Ought) b.getFormula();
            Formula precondition = ought.getPrecondition();
            Value outerTime = b.getTime();
            Value agent = b.getAgent();
            Belief preConditionBelief = new Belief(agent, outerTime, precondition);
            CCProver cognitiveCalculusProver = new CCProver(this);
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
        Set<Common> commons = CommonUtils.level2FormulaeOfType(base, Common.class);
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
        Set<Knowledge> knows = CommonUtils.level2FormulaeOfType(base, Knowledge.class);

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
        Set<Common> commons = CommonUtils.level2FormulaeOfType(base, Common.class);
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
        Set<Common> commons = CommonUtils.level2FormulaeOfType(base, Common.class);

        for (Common c : commons) {
            Formula formula = c.getFormula();
            if (!added.contains(formula)) {
                base.add(formula);
                added.add(formula);
            }
        }


    }

    private void expandModalConjunctions(Set<Formula> base, Set<Formula> added) {

        Set<And> level2Ands = CommonUtils.level2FormulaeOfType(base, And.class);

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


        Set<Implication> level2Ifs = CommonUtils.level2FormulaeOfType(base, Implication.class);

        for (Implication implication : level2Ifs) {

            if (prohibited.contains(implication)) {
                continue;
            }

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            CCProver cognitiveCalculusProver = new CCProver(this);
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

                            mapping.put(vars[i], substitution.get(vars.length - 1 - i));


                        }

                        Formula derived = universal.getArgument().apply(mapping);

                        if (!added.contains(derived)) {
                            base.add(derived);
                            added.add(derived);
                        }

                    }

            );


        });

    }

    private Set<Formula> shadow(Set<Formula> formulas) {
        return formulas.stream().map(f -> f.shadow(1)).collect(Collectors.toSet());
    }


    private static void expansionLog(String principle, Set<Formula> newSet) {
        if(!verbose) return;

        if (!newSet.isEmpty()) {

            coloredPrinter.print(indent+ "Forward Reasoning: ", Ansi.Attribute.BOLD, Ansi.FColor.BLUE, Ansi.BColor.NONE);
            coloredPrinter.print(principle, Ansi.Attribute.BOLD, Ansi.FColor.WHITE, Ansi.BColor.BLUE);
            coloredPrinter.clear();
            coloredPrinter.print(newSet, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            coloredPrinter.println("");
        }
        else {

        }

    }

    private static void tryLog(String principle, Formula goal) {
        if(!verbose) return;


            coloredPrinter.print(indent+ "Backward Reasoning: ", Ansi.Attribute.BOLD, Ansi.FColor.GREEN, Ansi.BColor.NONE);
            coloredPrinter.print(principle, Ansi.Attribute.BOLD, Ansi.FColor.BLACK, Ansi.BColor.GREEN);
            coloredPrinter.clear();
            coloredPrinter.print(goal, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            coloredPrinter.println("");

    }

    private static void tryAgentClosure(Formula formula){
                if(!verbose) return;

        coloredPrinter.clear();
        coloredPrinter.print(indent+ "Trying", Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);

         coloredPrinter.print(" Agent Closure: ", Ansi.Attribute.BOLD, Ansi.FColor.WHITE, Ansi.BColor.BLACK);
            coloredPrinter.clear();
            coloredPrinter.print(formula, Ansi.Attribute.NONE, Ansi.FColor.BLACK, Ansi.BColor.NONE);
            coloredPrinter.println("");
    }

    private static void logFOLCall(boolean success, Set<Formula> newSet, Formula goal) {

        if(!verbose) return;
        if(success){

            coloredPrinter.print(indent+ "First-Order Prover Call: ", Ansi.Attribute.BOLD, Ansi.FColor.MAGENTA, Ansi.BColor.NONE);
            coloredPrinter.print("Succeeded", Ansi.Attribute.BOLD, Ansi.FColor.GREEN, Ansi.BColor.NONE);
            coloredPrinter.println("");

        } else{

            coloredPrinter.print("First-Order Prover Call: ", Ansi.Attribute.BOLD, Ansi.FColor.MAGENTA, Ansi.BColor.NONE);
            coloredPrinter.print("Failed", Ansi.Attribute.BOLD, Ansi.FColor.RED, Ansi.BColor.NONE);
            coloredPrinter.println("");

        }

    }

}
