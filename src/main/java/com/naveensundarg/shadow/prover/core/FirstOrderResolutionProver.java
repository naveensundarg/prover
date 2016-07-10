package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.rule.DemodulationImplementation;
import com.naveensundarg.shadow.prover.core.rule.FirstOrderResolutionImplementation;
import com.naveensundarg.shadow.prover.core.rule.ParamodulationImplementation;
import com.naveensundarg.shadow.prover.core.rule.RuleImplementation;
import com.naveensundarg.shadow.prover.representations.Formula;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newList;
import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;
import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;
import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/12/16.
 */
public class FirstOrderResolutionProver implements Prover {

    private enum Rule {

        RESOLUTION(FirstOrderResolutionImplementation.INSTANCE),
        DEMODULATION(DemodulationImplementation.INSTANCE),
        PARAMODULATION(ParamodulationImplementation.INSTANCE);

        private RuleImplementation ruleImplementation;

        Rule(RuleImplementation ruleImplementation) {
            this.ruleImplementation = ruleImplementation;
        }

        public RuleImplementation getRuleImplementation() {
            return ruleImplementation;
        }
    }

    private final Map<Problem, Set<Pair<Clause, Clause>>> used;
    private final Set<Rule> rules;

    public FirstOrderResolutionProver(Set<Rule> rules) {

        used = newMap();
        this.rules = rules;
    }

    public FirstOrderResolutionProver() {

        used = newMap();
        this.rules = Sets.with(Rule.RESOLUTION);
        this.rules.add(Rule.PARAMODULATION);

    }


    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        Problem problem = new Problem();
        used.put(problem, newSet());

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

        int size = 1;
        while (size <= clauses.stream().mapToInt(x -> x.getLiterals().size()).max().getAsInt()) {


            List<List<Clause>> matchingPairs = getUsableClauses(clauses, problem, size);

            if (!matchingPairs.isEmpty()) {
                boolean expanded = false;

                for (List<Clause> pair : matchingPairs) {

                    Clause left = pair.get(0);
                    Clause right = pair.get(1);

                    Set<Clause> resolvands = rules.
                            stream().
                            map(ruleType -> ruleType.getRuleImplementation().apply(left, right)).
                            reduce(newSet(), Sets::union);

                    if (!resolvands.isEmpty()) {

                        List<Clause> resolvandsList = new ArrayList<>();
                        resolvands.stream().forEach(resolvandsList::add);

                        for (int j = 0; j < resolvandsList.size(); j++) {
                            Clause resolvand = resolvandsList.get(j);

                            if (resolvand.getLiterals().isEmpty()) {
                                return Optional.of(Justification.trivial(formula));
                            } else {
                                if (!clauses.contains(resolvand)) {
                                    clauses.add(resolvand);
                                    used.put(problem, Sets.add(used.get(problem), ImmutablePair.from(left, right)));
                                    expanded = true;
                                }
                            }
                        }
                    }
                }

                if (!expanded) {
                    size++;

                }

            } else {

                size++;

            }

        }

        return Optional.empty();

    }

    public List<List<Clause>> getUsableClauses(Set<Clause> clauses, Problem problem, int size) {

        List<Set<Clause>> sets = newList();
        sets.add(clauses);
        sets.add(clauses);

        Set<List<Clause>> possiblePairs = cartesianProduct(sets);

        return possiblePairs.stream().filter(possiblePair -> {
            Clause left = possiblePair.get(0);
            Clause right = possiblePair.get(1);

            if (used.get(problem).contains(ImmutablePair.from(left, right)) || right.getLiterals().size() > size) {
                return false;
            } else {
                return true;
            }
        }).collect(Collectors.toList());


    }


}

