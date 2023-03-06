package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.expanders.cognitivecalculus.*;
import com.naveensundarg.shadow.prover.core.expanders.inductivecalculus.AntiUnification;
import com.naveensundarg.shadow.prover.core.expanders.inductivecalculus.Generalize;
import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.core.internals.ConsistentSubsetFinder;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.AtomicJustification;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.*;
import java.util.stream.Collectors;


public class CognitiveCalculusProver implements CCProver {

    /*
     *
     */
    private static int            MAX_EXPAND_FACTOR    = 500;
    private        boolean        verbose              = true;
    private final  boolean        reductio;
    private final  boolean        theoremsToNec        = false;
    private final  List<Expander> expanders;
    private        Set<Formula>   prohibited;


    protected Logger logger;

    public CognitiveCalculusProver() {

        prohibited = Sets.newSet();
        reductio = false;
        expanders = CollectionUtils.newEmptyList();

        expanders.add(Generalize.INSTANCE);
        expanders.add(AntiUnification.INSTANCE);

        expanders.add(BreakupBiConditionals.INSTANCE);
        expanders.add(R4.INSTANCE);
        expanders.add(SelfBelief.INSTANCE);
        expanders.add(PerceptionToKnowledge.INSTANCE);
        expanders.add(SaysToBelief.INSTANCE);
        expanders.add(IntentionToPerception.INSTANCE);
        expanders.add(ModalConjunctions.INSTANCE);
        expanders.add(ModalImplications.INSTANCE);

        expanders.add(DR1.INSTANCE);
        expanders.add(DR2.INSTANCE);
        expanders.add(DR3.INSTANCE);
        expanders.add(DR5.INSTANCE);

        expanders.add(OughtSchema.INSTANCE);

        expanders.add(UniversalElim.INSTANCE);
        expanders.add(KnowledgeConjunctions.INSTANCE);

        expanders.add(NotExistsToForallNot.INSTANCE);

        expanders.add(NecToPos.INSTANCE);

        expanders.add(EqualitySubstitution.INSTANCE);
        expanders.add(NegatedConditionals.INSTANCE);
        expanders.add(SaysR1.INSTANCE);

        expanders.add(InnerModalForward.INSTANCE);
        if (theoremsToNec) {
            expanders.add(TheoremsToNecessity.INSTANCE);
        }
        logger = new Logger();
    }


    public CognitiveCalculusProver(CognitiveCalculusProver parent) {

        prohibited = CollectionUtils.setFrom(parent.prohibited);
        reductio = false;
        expanders = parent.expanders;

        this.verbose = parent.verbose;
        this.logger = parent.logger;

    }

    private CognitiveCalculusProver(CognitiveCalculusProver parent, boolean reductio) {

        prohibited = CollectionUtils.setFrom(parent.prohibited);
        this.reductio = reductio;
        expanders = parent.expanders;
        this.verbose = parent.verbose;
        this.logger = parent.logger;
    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        assumptions.forEach(x -> {

            if (x.getJustification() == null) {

                x.setJustification(new AtomicJustification("GIVEN"));
            }
        });
        return prove(assumptions, formula, CollectionUtils.newEmptySet());
    }


    public synchronized Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> added) {


        Prover folProver = SnarkWrapper.getInstance();

        Set<Formula> base = CollectionUtils.setFrom(assumptions);

        Formula shadowedGoal = formula.shadow(1);

        Optional<Justification> shadowedJustificationOpt = folProver.prove(shadow(base), shadowedGoal);

        logger.addContext();
        logger.tryAgentClosure(formula);
        Optional<Justification> agentClosureJustificationOpt = this.proveAgentClosure(base, formula);
        logger.removeContext();

        while (!shadowedJustificationOpt.isPresent() && !agentClosureJustificationOpt.isPresent()) {

            int sizeBeforeExpansion = base.size();
            base = expand(base, added, formula);
            int sizeAfterExpansion = base.size();

            if (sizeAfterExpansion > MAX_EXPAND_FACTOR * assumptions.size()) {
                return Optional.empty();
            }
            if (base.contains(formula)) {
                return Optional.of(TrivialJustification.trivial(base, formula));
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
        //   logFOLCall(false, shadow(base), shadowedGoal);

        return agentClosureJustificationOpt;

    }

    protected Optional<Justification> tryIfIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Implication) {

            Implication implication = (Implication) formula;

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            logger.addContext();
            logger.tryLog("Tying if intro", formula);
            Optional<Justification> consOpt = this.prove(Sets.add(base, antecedent), consequent);
            logger.removeContext();

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

    protected Optional<Justification> tryCounterFactIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof CounterFactual) {

            CounterFactual counterFactual = (CounterFactual) formula;

            Formula antecedent = counterFactual.getAntecedent();
            Formula consequent = counterFactual.getConsequent();

            logger.addContext();
            logger.tryLog("Tying counterfactual intro", formula);
            Optional<Justification> counterfactualIntroOpt = (new ConsistentSubsetFinder()).find(this, base, antecedent, consequent);
            logger.removeContext();

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

    protected Optional<Justification> tryExistsIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Existential) {

            Existential existential = (Existential) formula;
            Variable[]  vars        = existential.vars();

            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());

                logger.tryLog("Trying to prove existential", formula);
                return this.prove(base, ((Existential) formula).getArgument().apply(subs));

            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }


        } else {

            return Optional.empty();
        }
    }

    protected Optional<Justification> tryForAllIntro(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Universal) {

            Universal  universal = (Universal) formula;
            Variable[] vars      = universal.vars();

            logger.tryLog("Trying to prove universal", universal);
            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());
                //TODO: Verify this.
                Optional<Justification> ansOpt = this.prove(base, ((Universal) formula).getArgument().apply(subs));

                if (ansOpt.isPresent()) {

                    return Optional.of(new CompoundJustification("ForAllIntro", CollectionUtils.listOf(ansOpt.get())));
                } else {

                    return Optional.empty();
                    //TODO: Handle more than one variable
                }
            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }


        } else if (formula instanceof Not && ((Not) formula).getArgument() instanceof Existential) {

            //formula = (not (exists [vars] kernel)) == (forall [vars] (not kernel))
            Formula    kernel = ((Existential) ((Not) formula).getArgument()).getArgument();
            Variable[] vars   = ((Existential) ((Not) formula).getArgument()).vars();


            if (vars.length == 1) {

                Map<Variable, Value> subs = CollectionUtils.newMap();
                subs.put(vars[0], Constant.newConstant());
                logger.tryLog("Trying to prove ", (new Not(kernel)).apply(subs));

                return this.prove(base, (new Not(kernel)).apply(subs));

            } else {

                return Optional.empty();
                //TODO: Handle more than one variable
            }
        } else {

            return Optional.empty();
        }
    }

    protected Optional<Justification> tryNEC(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Necessity) {

            logger.tryLog("Trying to prove necessity", formula);
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

    protected Optional<Justification> tryPOS(Set<Formula> base, Formula formula, Set<Formula> added) {
        if (formula instanceof Not && ((Not) formula).getArgument() instanceof Possibility) {

            Formula core = ((Possibility) ((Not) formula).getArgument()).getFormula();

            logger.tryLog("Trying to prove necessity", new Necessity(new Not(core)));

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

    protected Optional<Justification> tryAND(Set<Formula> base, Formula formula, Set<Formula> added) {

        if (formula instanceof And) {

            And and = (And) formula;
            logger.tryLog("Trying to prove conjunction", and);


            Formula conjuncts[] = and.getArguments();

            List<Optional<Justification>> conjunctProofsOpt = Arrays.stream(conjuncts).map(conjunct -> {

                CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);
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



    protected Optional<Justification> tryOR(Set<Formula> base, Formula formula, Set<Formula> added) {

        Set<Or> level2ORs = CommonUtils.level2FormulaeOfType(base, Or.class);

        Optional<Or> someOrOpt = level2ORs.stream().findAny();

        if (someOrOpt.isPresent()) {

            Or        someOr    = someOrOpt.get();
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
                return Optional.of(new CompoundJustification("ORIntro", casesOpt.stream().map(Optional::get).collect(Collectors.toList())));
            } else {
                return Optional.empty();
            }

        } else {

            return Optional.empty();
        }

    }

    protected Optional<Justification> tryReductio(Set<Formula> base, Formula formula, Set<Formula> added) {

        Formula negated = Logic.negated(formula);
        if (base.contains(negated) || formula.toString().startsWith("$")) {
            return Optional.empty();
        }

        Atom atom = Atom.generate();

        logger.tryLog("Reductio on", negated);


        Set<Formula> augmented = CollectionUtils.setFrom(base);

        augmented.add(negated);
        logger.addContext();
        CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this, true);
        Optional<Justification> reductioJustOpt         = cognitiveCalculusProver.prove(augmented, atom, added);
        logger.removeContext();

        return reductioJustOpt.isPresent() ? Optional.of(new CompoundJustification("Reductio", CollectionUtils.listOf(reductioJustOpt.get()))) :
               Optional.empty();
    }

    Optional<Justification> proveAgentClosure(Set<Formula> base, Formula goal) {

       if(goal instanceof UnaryModalFormula) {
           return snapShotGoalProveInternal(base, (UnaryModalFormula) goal);
       }

        return Optional.empty();

    }

    private void expandInner(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {

        if(goal instanceof UnaryModalFormula) {
            UnaryModalFormula formula = (UnaryModalFormula) goal;
            Value agent         = formula.getAgent();
            Value    time          = formula.getTime();
            Formula  innerGoalFormula = formula.getFormula();

            AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

            Set<Formula> innerGivens = agentSnapShot.allBelievedByAgentTillTime(agent, time);



        }
    }

    private Optional<Justification> snapShotGoalProveInternal(Set<Formula> base, UnaryModalFormula formula) {

        Value agent         = formula.getAgent();
        Value    time          = formula.getTime();
        Formula  innerGoalFormula = formula.getFormula();

        AgentSnapShot agentSnapShot = AgentSnapShot.from(base);

        Set<Formula> innerGivens = Sets.newSet();




        if(formula instanceof Knowledge){
            innerGivens = agentSnapShot.allKnownByAgentTillTime(agent, time);
        }

        if(formula instanceof Belief){
            innerGivens = agentSnapShot.allBelievedByAgentTillTime(agent, time);
        }

        if(formula instanceof Intends){
            innerGivens = agentSnapShot.allIntendedByAgentTillTime(agent, time);
        }




        CognitiveCalculusProver cognitiveCalculusProver = new CognitiveCalculusProver(this);
        Optional<Justification> inner                   = cognitiveCalculusProver.prove(innerGivens, innerGoalFormula);



        return inner.map(justification -> new CompoundJustification(formula.getClass().toString(), CollectionUtils.listOf(justification)));
    }

    public Set<Formula> expand(Set<Formula> base, Set<Formula> added, Formula goal) {

        expanders.forEach(expander -> expander.expand(this, base, added, goal));

        if (prohibited != null) {
            base.removeAll(prohibited);
        }

        return base;
    }


    protected Set<Formula> shadow(Set<Formula> formulas) {
        return formulas.stream().map(f -> f.shadow(1)).collect(Collectors.toSet());
    }

    @Override
    public Logger getLogger() {
        return logger;
    }

    public Set<Formula> getProhibited() {
        return prohibited;
    }

}
