package com.naveensundarg.shadow.prover.core.oscar;

import com.naveensundarg.shadow.prover.core.Node;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Comparator;
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 12/19/16.
 */
public class NProver implements Prover {


    private final Set<Node> allNodes;
    private final PriorityQueue<Node> inferenceQueue;
    private final Set<Node> ultimateInterests;

    public NProver(){

        allNodes = Sets.newSet();
        ultimateInterests = Sets.newSet();
        inferenceQueue = CollectionUtils.newPriorityQueue(Comparator.comparingInt(x -> x.getFormula().subFormulae().size()));
    }


    private void prepareWorkSpace(Set<Formula> assumptions, Formula formula){
        Set<Node> assumptionNodes = assumptions.stream().map(Node::newAssumption).collect(Collectors.toSet());



    }


    private final boolean allUlimateInterestsDischarged(){

        return false;
    }
    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {


return null;

    }
}
