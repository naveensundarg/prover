package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Logic;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 12/13/16.
 */
public class NDProver implements Prover {


    private final Set<Formula> expanded;

    private final Stack<Formula> context;

    public NDProver() {

        this.expanded = Sets.newSet();
        this.context = new Stack<>();
    }


    public NDProver(NDProver parent) {

        this.expanded = CollectionUtils.setFrom(parent.expanded);
        this.context = new Stack<>();


    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {


        Set<Node> assumptionNodes = assumptions.stream().map(assumption -> new Node(assumption, NDRule.ASSUMPTION)).collect(Collectors.toSet());


        WorkSpace workSpace = WorkSpace.createWorkSpaceFromAssumptions(assumptions);
        Optional<Node> provedOpt = prove(workSpace, assumptions, formula);


        if (provedOpt.isPresent()) {
            return Optional.of(new TrivialJustification(formula));

        } else {
            return Optional.empty();
        }
    }


    private Optional<Node> prove(WorkSpace workSpace, Set<Formula> assumptions, Formula formula) {


        int currentSize = workSpace.size(), previousSize = workSpace.size();

        do {
            previousSize = workSpace.size();
            andElim(workSpace);
            ifElim(workSpace, assumptions, formula);
            orElim(workSpace, assumptions, formula);

            Optional<Node> trivialOpt = workSpace.fetch(assumptions, formula);

            if (trivialOpt.isPresent()) {
                return trivialOpt;
            }
            currentSize = workSpace.size();

        } while (currentSize != previousSize);


        Optional<Node> trivialOpt = workSpace.fetch(assumptions, formula);

        if (trivialOpt.isPresent()) {
            return trivialOpt;
        }


        Optional<Node> andIntroOpt = tryAndIntro(workSpace, assumptions, formula);

        if (andIntroOpt.isPresent()) {
            return andIntroOpt;
        }

        Optional<Node> orIntroOpt = tryOrIntro(workSpace, assumptions, formula);

        if (orIntroOpt.isPresent()) {
            return orIntroOpt;
        }

        Optional<Node> ifIntroOpt = tryIfIntro(workSpace, assumptions, formula);

        if (ifIntroOpt.isPresent()) {
            return ifIntroOpt;
        }

        Optional<Node> reductioOpt = tryReductio(workSpace, assumptions, formula);

        if (reductioOpt.isPresent()) {
            return reductioOpt;
        }


        return Optional.empty();
    }

    private void andElim(WorkSpace workSpace) {

        Set<Node> newNodes = workSpace.
                getNodes().
                stream().
                filter(node -> !workSpace.getExpanded().contains(node.getFormula())).
                filter(node -> node.getFormula() instanceof And).
                flatMap(node -> {
                    And and = (And) node.getFormula();
                    Formula[] conjuncts = and.getArguments();

                    return Arrays.stream(conjuncts).map(conjunct -> new Node(conjunct, NDRule.AND_ELIM, CollectionUtils.listOf(node)));

                }).collect(Collectors.toSet());

        workSpace.getNodes().addAll(newNodes);


    }

    private void ifElim(WorkSpace workSpace, Set<Formula> assumptions, Formula goal) {


        Set<Node> newNodes = workSpace.
                getNodes().
                stream().
                filter(node -> !workSpace.alreadyExpanded(node.getFormula())).
                filter(node -> node.getFormula() instanceof Implication).collect(Collectors.toSet());


        Set<Node> toBeAdded = newNodes.stream().
                map(node -> {
                    Implication implication = (Implication) node.getFormula();
                    Formula antecedent = implication.getAntecedent();
                    Formula consequent = implication.getConsequent();


                    workSpace.addToExpanded(implication);

                    Optional<Node> antecedentProvedOpt = prove(workSpace, assumptions, antecedent);

                    if (antecedentProvedOpt.isPresent()) {

                        workSpace.addNode(antecedentProvedOpt.get());
                        Node consequentNode = new Node(consequent, NDRule.IF_ELIM, CollectionUtils.listOf(antecedentProvedOpt.get()));


                        return consequentNode;


                    }

                    workSpace.getExpanded().remove(implication);


                    return null;


                }).filter(Objects::nonNull).collect(Collectors.toSet());

        workSpace.getNodes().addAll(toBeAdded);


    }


    private void orElim(WorkSpace workSpace, Set<Formula> assumptions, Formula goal) {


        Set<Node> newNodes = workSpace.
                getNodes().
                stream().
                filter(node -> !workSpace.alreadyExpanded(node.getFormula())).
                filter(node -> Sets.difference(node.getDerivedFrom(), assumptions).size() == 0).
                filter(node -> node.getFormula() instanceof Or).collect(Collectors.toSet());


        Set<Node> toBeAdded = newNodes.stream().
                map(node -> {
                    Or or = (Or) node.getFormula();

                    Formula[] disjuncts = or.getArguments();

                    workSpace.addToExpanded(or);

                    List<Node> provedNodes = CollectionUtils.newEmptyList();
                    for (int i = 0; i < disjuncts.length; i++) {


                        Node disjunctNode = Node.newAssumption(disjuncts[i]);
                        workSpace.addNode(disjunctNode);


                        Optional<Node> provedConsequentOpt = prove(workSpace, Sets.add(assumptions, disjuncts[i]), goal);


                        if (!provedConsequentOpt.isPresent()) {
                            workSpace.getExpanded().remove(or);
                            return null;
                        } else {

                            provedNodes.add(provedConsequentOpt.get());

                        }


                    }
                    for (int i = 0; i < disjuncts.length; i++) {

                        workSpace.addNode(provedNodes.get(i));
                    }

                    provedNodes.add(node);

                    Node finalNode = new Node(goal, NDRule.OR_ELIM, provedNodes);
                    workSpace.addNode(finalNode);


                    workSpace.getExpanded().remove(or);


                    return finalNode;


                }).filter(Objects::nonNull).collect(Collectors.toSet());

        workSpace.getNodes().addAll(toBeAdded);


    }

    private Optional<Node> tryAndIntro(WorkSpace workSpace, Set<Formula> assumptions, Formula formula) {

        if (formula instanceof And) {
            And and = (And) formula;

            Formula[] conjuncts = and.getArguments();

            List<Node> conjunctsProved = CollectionUtils.newEmptyList();
            for (int i = 0; i < conjuncts.length; i++) {

                Formula conjunct = conjuncts[i];

                Optional<Node> provedConjunct = prove(workSpace, assumptions, conjunct);

                if (!provedConjunct.isPresent()) {
                    return Optional.empty();
                } else {
                    conjunctsProved.add(provedConjunct.get());
                }


            }

            return Optional.of(new Node(formula, NDRule.AND_INTRO, conjunctsProved));

        }

        return Optional.empty();

    }


    private Optional<Node> tryOrIntro(WorkSpace workSpace, Set<Formula> assumptions, Formula formula) {

        if (formula instanceof Or) {
            Or or = (Or) formula;

            Formula[] disjuncts = or.getArguments();

            List<Node> disjunctProved = CollectionUtils.newEmptyList();

            for (int i = 0; i < disjuncts.length; i++) {

                Formula disjunct = disjuncts[i];

                Optional<Node> provedDisjunctOpt = prove(workSpace, assumptions, disjunct);

                if (provedDisjunctOpt.isPresent()) {

                    disjunctProved.add(provedDisjunctOpt.get());
                    Node node = new Node(formula, NDRule.OR_INTRO, disjunctProved);
                    workSpace.addNode(node);

                    return Optional.of(node);

                }


            }


        }

        return Optional.empty();

    }

    private Optional<Node> tryIfIntro(WorkSpace workSpace, Set<Formula> assumptions, Formula formula) {


        if (formula instanceof Implication) {
            Implication implication = (Implication) formula;

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            workSpace.assume(antecedent);

            Optional<Node> provedConsequent = prove(workSpace, Sets.add(assumptions, antecedent), consequent);

            if (provedConsequent.isPresent()) {

                List<Node> proved = CollectionUtils.newEmptyList();
                proved.add(provedConsequent.get());
                Node implicationNode = new Node(implication, NDRule.IF_INTRO, proved);

                workSpace.addNode(implicationNode);
                return Optional.of(implicationNode);
            }


        }

        return Optional.empty();


    }


    private Optional<Node> tryReductio(WorkSpace workSpace, Set<Formula> assumptions, Formula formula) {
        Formula negated = Logic.negated(formula);

        if(workSpace.isReductioBeingTriedOn(formula) || workSpace.hasAlreadyFailed(assumptions, formula)){
            return Optional.empty();
        }



        workSpace.addToCurrentReductioSet(formula);

        Node negatedNode = Node.newAssumption(negated);

        Set<Formula> atomicFormulae =
                assumptions.stream().
                        map(Formula::subFormulae).reduce(Sets.newSet(), Sets::union).stream().
                        filter(x -> x instanceof Atom || x instanceof Predicate).collect(Collectors.toSet());


        Set<Formula> absurds = atomicFormulae.stream().map(x -> new And(x, Logic.negated(x))).collect(Collectors.toSet());

        workSpace.addNode(negatedNode);

        absurds = Sets.union(absurds, assumptions.stream().map(Logic::negated).collect(Collectors.toSet()));

        for (Formula absurd : absurds) {


            Optional<Node> provedConsequent = prove(workSpace, Sets.add(assumptions, negated), absurd);

            if(provedConsequent.isPresent()){

                Node proved = new Node(formula, NDRule.NOT_ELIM, CollectionUtils.listOf(provedConsequent.get()), negated);

                workSpace.addNode(proved);

                workSpace.removeFromCurrentReductioSet(formula);
                return Optional.of(proved);

            }

        }

        workSpace.removeFromCurrentReductioSet(formula);

        workSpace.addToAlreadyFailed(assumptions, formula);

        return Optional.empty();

    }

}
