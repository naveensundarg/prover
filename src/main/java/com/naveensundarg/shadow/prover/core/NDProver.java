package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;
import com.naveensundarg.shadow.prover.utils.Visualizer;

import java.io.FileNotFoundException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 12/13/16.
 */
public class NDProver implements Prover {


    public boolean visualize = false;
    public NDProver() {

    }





    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {



        WorkSpace workSpace = WorkSpace.createWorkSpaceFromGiven(assumptions);

        Optional<Node> provedOpt = prove(workSpace, assumptions, formula);


        if (provedOpt.isPresent()) {


            if(visualize){
                try {
                    Visualizer.renderToDot(provedOpt.get());
                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                }
            }

            return Optional.of(new TrivialJustification(assumptions, formula));

        } else {
            return Optional.empty();
        }
    }


    private Optional<Node> prove(WorkSpace workSpace, Set<Formula> assumptions, Formula formula) {


        int currentSize, previousSize;


        do {
            previousSize = workSpace.size();
            andElim(workSpace);
            ifElim(workSpace, assumptions, formula);
            iffElim(workSpace, assumptions, formula);

            orElim(workSpace, assumptions, formula);

            Optional<Node> trivialOpt1 = workSpace.fetch(assumptions, formula);

            if (trivialOpt1.isPresent()) {
                return trivialOpt1;
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

        Optional<Node> iffIntroOpt = tryIffIntro(workSpace, assumptions, formula);

        if (iffIntroOpt.isPresent()) {
            return iffIntroOpt;
        }


        Optional<Node> reductioOpt = tryReductio(workSpace, assumptions, formula);

        if (reductioOpt.isPresent()) {
            return reductioOpt;
        }
        do {
            previousSize = workSpace.size();
            andElim(workSpace);
            ifElim(workSpace, assumptions, formula);
            iffElim(workSpace, assumptions, formula);

            orElim(workSpace, assumptions, formula);

            Optional<Node> trivialOpt1 = workSpace.fetch(assumptions, formula);

            if (trivialOpt1.isPresent()) {
                return trivialOpt1;
            }
            currentSize = workSpace.size();

        } while (currentSize != previousSize);


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

                        workSpace.assume(antecedentProvedOpt.get());
                        List<Node> parents = CollectionUtils.listOf(antecedentProvedOpt.get());
                        parents.add(node);
                        Node consequentNode = new Node(consequent, NDRule.IF_ELIM, parents);
                        workSpace.addNode(consequentNode);

                        return consequentNode;


                    } else{


                        Node newAssumption = Node.newAssumption(antecedent);
                        workSpace.assume(newAssumption);
                        List<Node> parents = CollectionUtils.listOf(newAssumption);
                        parents.add(node);
                        Node consequentNode = new Node(consequent, NDRule.IF_ELIM, parents);

                        workSpace.getExpanded().remove(implication);

                        return consequentNode;


                    }





                }).filter(Objects::nonNull).collect(Collectors.toSet());

        workSpace.getNodes().addAll(toBeAdded);


    }


    private void iffElim(WorkSpace workSpace, Set<Formula> assumptions, Formula goal) {


        Set<Node> newNodes = workSpace.
                getNodes().
                stream().
                filter(node -> !workSpace.alreadyExpanded(node.getFormula())).
                filter(node -> node.getFormula() instanceof BiConditional).collect(Collectors.toSet());


        Set<Node> rightToBeAdded = newNodes.stream().
                map(node -> {
                    BiConditional biConditional = (BiConditional) node.getFormula();
                    Formula left = biConditional.getLeft();
                    Formula right = biConditional.getRight();


                    workSpace.addToExpanded(biConditional);

                    Optional<Node> antecedentProvedOpt = prove(workSpace, assumptions, left);

                    if (antecedentProvedOpt.isPresent()) {

                        workSpace.addNode(antecedentProvedOpt.get());
                        Node consequentNode = new Node(right, NDRule.IFF_ELIM, CollectionUtils.listOf(antecedentProvedOpt.get(), node));
                        workSpace.getExpanded().remove(biConditional);

                        return consequentNode;


                    }else{


                        Node newAssumption = workSpace.assumeAndFetch(left);
                        List<Node> parents = CollectionUtils.listOf(newAssumption);
                        parents.add(node);
                        Node consequentNode = new Node(right, NDRule.IFF_ELIM, parents);

                        workSpace.getExpanded().remove(biConditional);

                        return consequentNode;


                    }


                }).filter(Objects::nonNull).collect(Collectors.toSet());


        Set<Node> leftToBeAdded = newNodes.stream().
                map(node -> {
                    BiConditional biConditional = (BiConditional) node.getFormula();
                    Formula left = biConditional.getLeft();
                    Formula right = biConditional.getRight();


                    workSpace.addToExpanded(biConditional);

                    Optional<Node> antecedentProvedOpt = prove(workSpace, assumptions, right);

                    if (antecedentProvedOpt.isPresent()) {

                        workSpace.addNode(antecedentProvedOpt.get());
                        Node consequentNode = new Node(left, NDRule.IFF_ELIM, CollectionUtils.listOf(antecedentProvedOpt.get(), node));

                        return consequentNode;


                    }else{


                        Node newAssumption = workSpace.assumeAndFetch(right);
                        List<Node> parents = CollectionUtils.listOf(newAssumption);
                        parents.add(node);
                        Node consequentNode = new Node(left, NDRule.IFF_ELIM, parents);

                        workSpace.getExpanded().remove(biConditional);

                        return consequentNode;


                    }





                }).filter(Objects::nonNull).collect(Collectors.toSet());

        workSpace.getNodes().addAll(leftToBeAdded);

        workSpace.getNodes().addAll(rightToBeAdded);


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
                        workSpace.assume(disjunctNode);


                        Optional<Node> provedConsequentOpt = prove(workSpace, Sets.add(assumptions, disjuncts[i]), goal);



                        if (!provedConsequentOpt.isPresent()) {
                            workSpace.getExpanded().remove(or);
                            return null;
                        }  else {

                            if(!provedConsequentOpt.get().getDerivedFrom().contains(disjuncts[i])){
                                return null;
                            }

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


                WorkSpace copyWorkSpace = workSpace.copy();

                Optional<Node> provedConjunct = prove(copyWorkSpace, assumptions, conjunct);

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

            WorkSpace workSpace1 = workSpace.copy();

            Node antecedentNode = new Node(antecedent, NDRule.GIVEN);
            workSpace1.addNode(antecedentNode);

            Optional<Node> provedConsequent = prove(workSpace1, Sets.add(assumptions, antecedent), consequent);

            antecedentNode.setNdRule(NDRule.ASSUMPTION);
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


    private Optional<Node> tryIffIntro(WorkSpace workSpace, Set<Formula> assumptions, Formula formula) {


        if (formula instanceof BiConditional) {
            BiConditional biConditional = (BiConditional) formula;

            Formula left = biConditional.getLeft();
            Formula right = biConditional.getRight();


            Formula leftImplication = new Implication(left, right);
            Formula rightImplication = new Implication(right, left);

            WorkSpace workSpace1 = workSpace.copy();
            WorkSpace workSpace2 = workSpace.copy();

            Optional<Node> provedConsequentRight = prove(workSpace1, assumptions, leftImplication);
            Optional<Node> provedConsequentLeft  = prove(workSpace2, assumptions, rightImplication);

            if (provedConsequentLeft.isPresent() && provedConsequentRight.isPresent()) {

                List<Node> proved = CollectionUtils.newEmptyList();
                proved.add(provedConsequentLeft.get());
                proved.add(provedConsequentRight.get());
                Node biConditionalNode = new Node(biConditional, NDRule.IFF_INTRO, proved);

                workSpace.addNode(biConditionalNode);

                return Optional.of(biConditionalNode);
            }




        }

        return Optional.empty();


    }

    private Optional<Node> tryReductio(WorkSpace workSpace, Set<Formula> assumptions, Formula formula) {
        Formula negated = Logic.negated(formula);


        Set<Node> relevantNodes  = workSpace.getNodes().stream().filter(node->Sets.subset(node.getDerivedFrom(),assumptions)).collect(Collectors.toSet());
        Set<Formula> relevantFormula = relevantNodes.stream().map(Node::getFormula).collect(Collectors.toSet());

        Optional<Node> alreadyOpt = relevantNodes.stream().filter(n-> relevantFormula.contains(Logic.negated(n.getFormula()))).findAny();

        if(alreadyOpt.isPresent()){

            Optional<Node> negatedAlreadyOpt = relevantNodes.stream().filter(n-> alreadyOpt.get().getFormula().equals(Logic.negated(n.getFormula()))).findAny();

            Node n1 = alreadyOpt.get();

            Node n2 = negatedAlreadyOpt.get();


            NDRule ndRule = formula instanceof Not? NDRule.NOT_INTRO:NDRule.NOT_ELIM;
            Node proved = new Node(formula, ndRule, CollectionUtils.listOf(n1,n2), negated);

            workSpace.addNode(proved);

            workSpace.removeFromCurrentReductioSet(formula);
            return Optional.of(proved);


        }

        if(workSpace.isReductioBeingTriedOn(formula) || workSpace.hasAlreadyFailed(assumptions, formula)){
            return Optional.empty();
        }



        workSpace.addToCurrentReductioSet(formula);

        Node negatedNode = Node.newAssumption(negated);

        Set<Formula> atomicFormulae =
                assumptions.stream().
                        map(Formula::subFormulae).reduce(Sets.newSet(), Sets::union).stream().
                        filter(x -> x instanceof Atom || x instanceof Predicate).collect(Collectors.toSet());


        Set<Formula> absurdTargets = atomicFormulae.stream().collect(Collectors.toSet());

        workSpace.assume(negatedNode);

        absurdTargets = Sets.union(absurdTargets, assumptions.stream().collect(Collectors.toSet()));

        for (Formula absurdTarget : absurdTargets) {


            Optional<Node> provedConsequent = prove(workSpace, Sets.add(assumptions, negated), absurdTarget);
            Optional<Node> provedConsequentNegated = prove(workSpace, Sets.add(assumptions, negated), Logic.negated(absurdTarget));

            if(provedConsequent.isPresent() && provedConsequentNegated.isPresent() && (provedConsequent.get().getDerivedFrom().contains(negated) || provedConsequentNegated.get().getDerivedFrom().contains(negated))){

                NDRule ndRule = formula instanceof Not? NDRule.NOT_INTRO:NDRule.NOT_ELIM;
                Node proved = new Node(formula, ndRule, CollectionUtils.listOf(provedConsequent.get(), provedConsequentNegated.get()), negated);

                workSpace.addNode(proved);

                workSpace.removeFromCurrentReductioSet(formula);
                return Optional.of(proved);

            }

        }

        workSpace.removeFromCurrentReductioSet(formula);
        workSpace.getNodes().remove(negatedNode);

        workSpace.addToAlreadyFailed(assumptions, formula);

        return Optional.empty();

    }

}
