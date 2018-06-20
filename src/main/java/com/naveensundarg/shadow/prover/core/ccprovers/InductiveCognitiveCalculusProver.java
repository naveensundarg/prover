package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.internals.FirstOrderAntiUnifier;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.core.Logic.instantiateActionType;


public class InductiveCognitiveCalculusProver extends CognitiveCalculusProver {

    /*
     *
     */
    private static boolean defeasible = false;
    private static boolean verbose = true;
    private final boolean reductio;
    private final boolean theoremsToNec = false;
    private final InductiveCognitiveCalculusProver parent;
    private final Set<Expander> expanders;
    private Set<Formula> currentAssumptions;
    private Set<Formula> prohibited;
    private Problem currentProblem;



    public InductiveCognitiveCalculusProver() {

        prohibited = Sets.newSet();
        parent = null;
        reductio = false;
        expanders = CollectionUtils.newEmptySet();
    }


    private static InductiveCognitiveCalculusProver root(InductiveCognitiveCalculusProver cognitiveCalculusProver) {


        InductiveCognitiveCalculusProver current = cognitiveCalculusProver.parent;

        if (current == null) {
            return cognitiveCalculusProver;
        }
        while (current.parent != null) {

            current = current.parent;
        }

        return current;

    }

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        return prove(assumptions, formula, CollectionUtils.newEmptySet());
    }


    private synchronized Optional<Justification> prove(Set<Formula> assumptions, Formula formula, Set<Formula> added) {


        currentAssumptions = assumptions;


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

            base = induce(base);
            base = activateTraits(base);
            base = transferTraits(base);

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


            return shadowedJustificationOpt;
        }

        if (agentClosureJustificationOpt.isPresent()) {

            return agentClosureJustificationOpt;
        }

        return Optional.empty();
    }

    private Set<Formula> transferTraits(Set<Formula> base) {

        Set<Formula> expanded = Sets.newSet();
        expanded.addAll(base);



        Set<Formula> admires =  base.stream().filter(x-> x instanceof Predicate && ((Predicate) x).getName().equals("Admire")).collect(Collectors.toSet());

        admires.stream().forEach(x->{


            Value a1 = ((Predicate) x).getArguments()[0];
            Value a2 = ((Predicate) x).getArguments()[1];

            Set<Trait> traits =  formulaOfType(base, Trait.class).stream().map(t-> (Trait) t).filter(t-> t.getAgent().equals(a2)).collect(Collectors.toSet());



            Set<Formula> newTraits = traits.stream().map(trait->
                            new Trait(trait.getTraitVariables(), a1, trait.getTime(),
                                    trait.getTriggeringCondition(),
                                    trait.getActionType())).collect(Collectors.toSet());
            expansionLog("transfering traits", newTraits);
            expanded.addAll(newTraits);

        });


        return expanded;

    }

    private Set<Formula> activateTraits(Set<Formula> base) {



        Set<Formula> expanded = Sets.newSet();
        expanded.addAll(base);

        Set<Trait> traits =  formulaOfType(base, Trait.class);

        traits.stream().forEach(trait -> {

            Formula triggeringCondition = trait.getTriggeringCondition();

            Prover prover = SnarkWrapper.getInstance();

            Optional<org.apache.commons.lang3.tuple.Pair<Justification, Set<Map<Variable, Value>>>> bindingsOpt =
                    prover.proveAndGetMultipleBindings(shadow(Sets.remove(base, trait)),  triggeringCondition.shadow(1), trait.getTraitVariables());

            if(bindingsOpt.isPresent()){

                //FIXME: Copy the formula;

                triggeringCondition.setJustification(bindingsOpt.get().getLeft());
                expanded.add(triggeringCondition);

                bindingsOpt.get().getRight().forEach(subst -> {

                    expanded.add(instantiateActionType(trait.getAgent(), trait.getTime(), trait.getActionType()).apply(subst));

                });

            }

        });



        return expanded;

    }


    private Set<Formula> induce(Set<Formula> base){


        Set<Formula> expanded = Sets.newSet();
        expanded.addAll(base);

        Set<Belief> beliefs =  level2FormulaeOfType(base, Belief.class);

        Set<Value> agents = beliefs.stream().map(Belief::getAgent).collect(Collectors.toSet());

        agents.forEach(agent->{

            List<Value> values = CollectionUtils.newEmptyList();


            beliefs.stream().filter(belief -> belief.getAgent().equals(agent)).forEach(belief -> {


                List<Value> matchingActions = base.stream().filter(formula ->
                                !formula.equals(belief.getFormula()) &&
                                formula instanceof Predicate &&
                                ((Predicate) formula).getName().equals("happens") &&
                                ((Predicate) formula).getArguments()[1].equals(
                                        new Compound("next",
                                                new Value[]{belief.getTime()}))).
                        map(f->((Predicate) f).getArguments()[0].getArguments()[1]).collect(Collectors.toList());

                try {
                    if(!matchingActions.isEmpty()){

                        values.add(new Compound("match", new Value[]{Reader.readLogicValueFromString(belief.getFormula().toString()),

                                Reader.readLogicValueFromString(matchingActions.get(0).toString())
                        }));
                    }

                } catch (Reader.ParsingException e) {
                    e.printStackTrace();
                }
            });

            if(values.size() >= 2){

                Value abstraction  =  FirstOrderAntiUnifier.antiUnify(values);


                try {

                    if(!abstraction.getArguments()[0].toString().startsWith("?")){

                        Trait trait = new Trait(new ArrayList<>(abstraction.variablesPresent()),
                                agent, Reader.NOW, Reader.readFormulaFromString(abstraction.getArguments()[0].toString()),
                                abstraction.getArguments()[1]);

                        expanded.add(trait);

                        System.out.println(trait);
                    }





                } catch (Reader.ParsingException e) {
                    e.printStackTrace();
                }
            }
        });


        return expanded;
    }









}
