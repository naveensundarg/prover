package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.internals.AgentSnapShot;
import com.naveensundarg.shadow.prover.core.internals.UniversalInstantiation;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.rules.DemodulationImplementation;
import com.naveensundarg.shadow.prover.core.rules.FirstOrderResolutionImplementation;
import com.naveensundarg.shadow.prover.core.rules.ForwardClauseRule;
import com.naveensundarg.shadow.prover.core.rules.ParamodulationImplementation;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;
import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;
import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/21/16.
 */
public class HaloCognitiveCalculusProver implements Prover {

    /*
     *
     */

    private enum Rule {

        RESOLUTION(FirstOrderResolutionImplementation.INSTANCE),
        DEMODULATION(DemodulationImplementation.INSTANCE),
        PARAMODULATION(ParamodulationImplementation.INSTANCE);

        private ForwardClauseRule forwardClauseRule;

        Rule(ForwardClauseRule forwardClauseRule) {
            this.forwardClauseRule = forwardClauseRule;
        }

        public ForwardClauseRule getForwardClauseRule() {
            return forwardClauseRule;
        }
    }

    private final Map<Problem, Set<Pair<Clause, Clause>>> used;
    private final Set<Rule> rules;

    public HaloCognitiveCalculusProver(Set<Rule> rules) {

        used = newMap();
        this.rules = rules;
    }

    public HaloCognitiveCalculusProver() {

        used = newMap();
        this.rules = Sets.with(Rule.RESOLUTION);
        this.rules.add(Rule.PARAMODULATION);

    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        int k = 5;


        Clause.count = new AtomicInteger(0);
        PriorityQueue<Clause> clauseStore = new PriorityQueue<>(Comparator.comparing(Clause::getWeight));
        Queue<Clause> ageQueue = new LinkedList<>();

        Problem problem = new Problem(assumptions, formula);

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
            clauseStore.add(clause);
            ageQueue.add(clause);
        }

        Set<Clause> usableList = CollectionUtils.newEmptySet();

        usableList.add(clauseStore.remove());

        int ageWeightCounter  = 0;

        while (!clauseStore.isEmpty() && !ageQueue.isEmpty()) {

            Clause given;


            if(ageWeightCounter%k == 0) {

                given = ageQueue.remove();
                clauseStore.remove(given);

            }
            else {
                given = clauseStore.remove();
                ageQueue.remove(given);

            }

            ageWeightCounter = ageWeightCounter + 1;


            if(usableList.stream().anyMatch(x->x.subsumes(given))){
                continue;
            }
            Clause renamedGiven = Logic.renameVars(given,problem);

            for (Clause parent : usableList) {


                Set<Clause> resolvands = rules.
                        stream().
                        map(ruleType -> ruleType.getForwardClauseRule().apply(parent, renamedGiven)).
                        reduce(newSet(), Sets::union);

                for (Clause resolvand : resolvands) {

                    if(usableList.stream().anyMatch(x->x.subsumes(resolvand))
                            || clauseStore.stream().anyMatch(x->x.subsumes(resolvand))
                            || ageQueue.stream().anyMatch(x->x.subsumes(resolvand)) ){
                        continue;
                    }



                    if (resolvand.getLiterals().isEmpty()) {

                        return Optional.of(Justification.trivial(formula));

                    } else {

                        if (!clauseStore.contains(resolvand) && !usableList.contains(resolvand)) {

                            clauseStore.add(resolvand);
                        }
                        if (!ageQueue.contains(resolvand) && !usableList.contains(resolvand)) {

                            ageQueue.add(resolvand);
                        }
                    }
                }


            }

            List<Clause> ageQueueProcessed = ageQueue.stream().filter(x->!given.subsumes(x)&& x.getWeight()<=15).collect(Collectors.toList());
            ageQueue.clear();
            for(Clause clause:ageQueueProcessed){

                ageQueue.add(clause);
            }

            List<Clause> clauseStoreProcessed = clauseStore.stream().filter(x->!given.subsumes(x) && x.getWeight()<=15).collect(Collectors.toList());
            clauseStore.clear();
            for(Clause clause:clauseStoreProcessed){

                clauseStore.add(clause);
            }



            usableList  = usableList.stream().filter(x->!given.subsumes(x)).collect(Collectors.toSet());
            usableList.add(given);


        }
        return Optional.empty();
    }

}
