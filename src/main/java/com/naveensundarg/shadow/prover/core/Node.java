package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 12/13/16.
 */
public class Node {

    private final Formula formula;

    public void setNdRule(NDRule ndRule) {
        this.ndRule = ndRule;
    }

    private  NDRule ndRule;
    private final List<Node> parents;

    private final Set<Formula> derivedFrom;
    public int id;


    private static AtomicInteger Node_ID_COUNTER = new AtomicInteger(0);

    private final Optional<Formula> removeFromBaseOpt;

    private  Node(Formula formula) {

        this.formula = formula;
        this.ndRule = NDRule.ASSUMPTION;
        this.parents = CollectionUtils.newEmptyList();

        this.derivedFrom = computeDerivedFrom();

        this.id = Node_ID_COUNTER.addAndGet(1);
        this.removeFromBaseOpt = Optional.empty();
    }


    Node(Formula formula, NDRule ndRule) {

        this.formula = formula;
        this.ndRule = ndRule;
        this.parents = CollectionUtils.newEmptyList();

        this.removeFromBaseOpt = Optional.empty();

        this.derivedFrom = computeDerivedFrom();
        this.id = Node_ID_COUNTER.addAndGet(1);


    }


    Node(Formula formula, NDRule ndRule, List<Node> parents) {

        this.formula = formula;
        this.ndRule = ndRule;
        this.parents = parents;
        if (ndRule.equals(NDRule.ASSUMPTION) && !parents.isEmpty()) {
            throw new AssertionError("Assumption nodes cannot have any parent nodes");
        }

        this.removeFromBaseOpt = Optional.empty();

        this.derivedFrom = computeDerivedFrom();
        this.id = Node_ID_COUNTER.addAndGet(1);


    }

    Node(Formula formula, NDRule ndRule, List<Node> parents, Formula removeFromBase) {

        this.formula = formula;
        this.ndRule = ndRule;
        this.parents = parents;
        if (ndRule.equals(NDRule.ASSUMPTION) && !parents.isEmpty()) {
            throw new AssertionError("Assumption nodes cannot have any parent nodes");
        }

        this.removeFromBaseOpt = Optional.of(removeFromBase);

        this.derivedFrom = computeDerivedFrom();
        this.id = Node_ID_COUNTER.addAndGet(1);

    }


    public static Node newAssumption(Formula formula){
        return new Node(formula);
    }


    public boolean formulaEquals(Formula f) {
        return this.formula.equals(f);
    }
    private Set<Formula> computeDerivedFrom(){
        if(ndRule.equals(NDRule.ASSUMPTION) || ndRule.equals(NDRule.GIVEN)){
            return Sets.with(formula);
        }
        else {

            Set<Formula> base = parents.stream().map(Node::computeDerivedFrom).reduce(Sets.newSet(), Sets::union);

            if(ndRule.equals(NDRule.IF_INTRO)){
                Implication implication = (Implication) formula;
                return Sets.remove(base, implication.getAntecedent());
            }

           /* if(ndRule.equals(NDRule.IFF_INTRO)){
                BiConditional biConditional = (BiConditional) formula;
                Set<Formula> newBase = Sets.remove(base, biConditional.getLeft());
                newBase = Sets.remove(newBase, biConditional.getRight());

                return newBase;
            }*/

            if(ndRule.equals(NDRule.OR_ELIM)){

                Or or  = (Or) parents.get(parents.size()-1).getFormula();

                return Sets.difference(base, Arrays.stream(or.getArguments()).collect(Collectors.toSet()));
            }

            if(ndRule.equals(NDRule.OR_ELIM)){

                Or or  = (Or) parents.get(parents.size()-1).getFormula();

                return Sets.difference(base, Arrays.stream(or.getArguments()).collect(Collectors.toSet()));
            }

            return removeFromBaseOpt.map(formula -> Sets.remove(base, formula)).orElse(base);

        }

    }

    public int getId() {
        return id;
    }
    public Formula getFormula() {
        return formula;
    }

    public NDRule getNdRule() {
        return ndRule;
    }

    public List<Node> getParents() {
        return parents;
    }


    public Set<Formula> getDerivedFrom() {
        return derivedFrom;
    }

    public List<Node> ancestors(){

        List<Node> ancestors = CollectionUtils.newEmptyList();

        ancestors.addAll(parents);

        for(Node parent: parents){

            ancestors.addAll(parent.ancestors());

        }
        return ancestors;
    }
    @Override
    public String toString() {
        return "[" + id +": " + formula + "; " + ndRule +"; " +  parents.stream().map(Node::getId).collect(Collectors.toSet()) +"]";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Node node = (Node) o;

        if (!formula.equals(node.formula)) return false;
        if (ndRule != node.ndRule) return false;
        return parents.equals(node.parents);
    }

    @Override
    public int hashCode() {
        int result = formula.hashCode();
        result = 31 * result + ndRule.hashCode();
        result = 31 * result + parents.hashCode();
        return result;
    }
}
