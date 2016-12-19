package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.ImmutablePair;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;
import jdk.nashorn.internal.ir.annotations.Immutable;

import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 12/13/16.
 */
public class WorkSpace {


    private final Set<Node> nodes;
    private final Set<Formula> expanded;
    private final AtomicBoolean inAReductioNow;
    private final Set<Formula> currentReductioSet;
    private final Set<Pair<Set<Formula>, Formula>> alreadyFailed;

    private WorkSpace() {
        this.nodes = Sets.newSet();
        this.expanded = Sets.newSet();
        this.currentReductioSet = Sets.newSet();
        this.inAReductioNow = new AtomicBoolean();
        this.alreadyFailed = Sets.newSet();
    }



    public static WorkSpace createEmptyWorkSpace() {
        return new WorkSpace();
    }





    public static WorkSpace createWorkSpaceFromGiven(Set<Formula> assumptions){

        Set<Node> assumptionNodes = assumptions.stream().map(assumption-> new Node(assumption, NDRule.GIVEN)).collect(Collectors.toSet());

        WorkSpace workSpace = new WorkSpace();

        workSpace.nodes.addAll(assumptionNodes);

        return workSpace;

    }

    /*** Already Seen ***/
    public void addToAlreadyFailed(Set<Formula> assumptions, Formula formula) {

        alreadyFailed.add(ImmutablePair.from(assumptions,formula));
    }

    public boolean hasAlreadyFailed(Set<Formula> assumptions, Formula formula){
        return alreadyFailed.stream().anyMatch(pair->{

            Set<Formula> seenAssumptions = pair.first();
            Formula seenFormula = pair.second();

            return Sets.subset(assumptions, seenAssumptions) && formula.equals(seenFormula);

        });
    }

    /*** Already Seen ***/
    public void assume(Formula formula){
        nodes.add(Node.newAssumption(formula));
    }

    public void assume(Node formula){
        nodes.add(formula);
    }

    public Optional<Node> fetch(Set<Formula> assumptions, Formula formula){

        return nodes.stream().filter(node-> node.formulaEquals(formula) && assumptions.containsAll(node.getDerivedFrom())).findAny();

    }

    public void addNode(Node node){


        nodes.add(node);

    }

    public void addToCurrentReductioSet(Formula formula){

        currentReductioSet.add(formula);

    }

    public Set<Formula> getCurrentReductioSet() {
        return currentReductioSet;
    }

    public void removeFromCurrentReductioSet(Formula formula){

        currentReductioSet.remove(formula);

    }

    public boolean isReductioBeingTriedOn(Formula formula){

        return currentReductioSet.contains(formula);

    }

    public Set<Node> getNodes() {
        return nodes;
    }

    public void addToExpanded(Formula formula){
        expanded.add(formula);
    }

    public boolean alreadyExpanded(Formula formula){
        return expanded.contains(formula);
    }

    public Set<Formula> getExpanded() {
        return expanded;
    }

    public Set<Pair<Set<Formula>, Formula>> getAlreadyFailed() {
        return alreadyFailed;
    }

    public boolean inReductio(){
        return inAReductioNow.get();
    }

    public void enterReductio(){

        inAReductioNow.set(true);
    }

    public void exitReductio(){
        inAReductioNow.set(false);
    }
    public int size() {
        return nodes.size();
    }
    @Override
    public String toString() {
        return "WorkSpace{" +
                "nodes=" + nodes +
                ", expanded=" + expanded +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        WorkSpace workSpace = (WorkSpace) o;

        if (!nodes.equals(workSpace.nodes)) return false;
        return expanded.equals(workSpace.expanded);
    }

    @Override
    public int hashCode() {
        int result = nodes.hashCode();
        result = 31 * result + expanded.hashCode();
        return result;
    }
}
