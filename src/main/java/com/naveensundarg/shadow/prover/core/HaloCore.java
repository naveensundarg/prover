package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.ccprovers.Converter;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.rules.ForwardClauseRule;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/12/16.
 */
public abstract class HaloCore implements Prover {



    public abstract Set<ForwardClauseRule> getForwardClauseRules();

    public abstract int getAgeWeightRatio();

    public abstract int getMaxWeight();

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        int k = getAgeWeightRatio();


        PriorityQueue<Clause> weightQueue = new PriorityQueue<>(Comparator.comparing(Clause::getWeight));

        Queue<Clause> ageQueue = new LinkedList<>();

        Problem problem = new Problem("", "", assumptions, formula);

        Set<CNFFormula> formulas = assumptions.
                stream().
                map(x -> Converter.convertToCNF(x, problem)).
                collect(Collectors.toSet());

        formulas.add(Converter.convertToCNF(Logic.negated(formula), problem));


        //TODO: Factoring!

        Set<Clause> clauses = formulas.
                stream().
                map(CNFFormula::getClauses).
                reduce(newSet(), Sets::union);

        clauses = clauses.stream().map(x -> Logic.renameVars(x, problem)).collect(Collectors.toSet());

        clauses = clauses.stream().map(Clause::refactor).collect(Collectors.toSet());


        for (Clause clause : clauses) {
            weightQueue.add(clause);
            ageQueue.add(clause);
        }

        Set<Clause> usableList = CollectionUtils.newEmptySet();

        usableList.add(weightQueue.remove());

        int ageWeightCounter  = 0;

        while (!weightQueue.isEmpty() && !ageQueue.isEmpty()) {

            Clause given;


            if(ageWeightCounter%k == 0) {

                given = ageQueue.remove();
                weightQueue.remove(given);

            }
             else {
                given = weightQueue.remove();
                ageQueue.remove(given);

            }

            ageWeightCounter = ageWeightCounter + 1;


            if(usableList.stream().anyMatch(x->x.subsumes(given))){
                continue;
            }
            Clause renamedGiven = Logic.renameVars(given,problem);

            for (Clause parent : usableList) {


                Set<Clause> resolvands = getForwardClauseRules().
                        stream().
                        map(rule -> rule.apply(parent, renamedGiven)).
                        reduce(newSet(), Sets::union);

                for (Clause resolvand : resolvands) {

                    if(usableList.stream().anyMatch(x->x.subsumes(resolvand))
                            || weightQueue.stream().anyMatch(x->x.subsumes(resolvand))
                            || ageQueue.stream().anyMatch(x->x.subsumes(resolvand)) ){
                        continue;
                    }



                    if (resolvand.getLiterals().isEmpty()) {

                        return Optional.of(Justification.trivial(assumptions, formula));

                    } else {

                        if (!weightQueue.contains(resolvand) && !usableList.contains(resolvand)) {

                            weightQueue.add(resolvand);
                        }
                        if (!ageQueue.contains(resolvand) && !usableList.contains(resolvand)) {

                            ageQueue.add(resolvand);
                        }
                    }
                }


            }


            List<Clause> ageQueueProcessed = ageQueue.stream().filter(x->!given.subsumes(x)&& x.getWeight()<= getMaxWeight()).collect(Collectors.toList());
            ageQueue.clear();
            for(Clause clause:ageQueueProcessed){

                ageQueue.add(clause);
            }

            List<Clause> clauseStoreProcessed = weightQueue.stream().filter(x->!given.subsumes(x) && x.getWeight()<= getMaxWeight()).collect(Collectors.toList());
            weightQueue.clear();
            for(Clause clause:clauseStoreProcessed){

                weightQueue.add(clause);
            }




            usableList  = usableList.stream().filter(x->!given.subsumes(x)).collect(Collectors.toSet());
            usableList.add(given);


        }
        return Optional.empty();
    }


}

