package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.Formula;
import com.naveensundarg.shadow.prover.representations.Predicate;
import com.naveensundarg.shadow.prover.representations.Value;
import com.naveensundarg.shadow.prover.representations.Variable;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.utils.ImmutablePair;
import com.naveensundarg.shadow.prover.utils.Logic;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

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

    Map<Problem, Set<Pair<Clause, Clause>>> used;

    public FirstOrderResolutionProver() {

        used = newMap();

    }

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        Problem problem = new Problem();
        Set<CNFFormula> formulas = assumptions.stream().
                map(x -> Converter.convertToCNF(x, problem)).
                collect(Collectors.toSet());

        used.put(problem, newSet());

        //TODO: Factoring!

        formulas.add(Converter.convertToCNF(Logic.negated(formula), problem));

        Set<Clause> clauses = formulas.stream().
                map(CNFFormula::getClauses).
                reduce(newSet(), Sets::union);


        while (true) {

            int size = 1;
            while(size <= clauses.stream().mapToInt(x->x.getLiterals().size()).max().getAsInt()){


                List<List<Clause>> matchingPairs = getMatchingClauses(clauses, problem, size);

                if(!matchingPairs.isEmpty()) {
                    boolean expanded = false;

                    for (List<Clause> pair : matchingPairs) {

                        Clause left = pair.get(0);
                        Clause right = pair.get(1);

                        Set<Clause> resolvands = resolve(left, right);


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

    }


    public Map<Variable, Value> matches(Literal literal1, Literal literal2) {

        boolean differ = (literal1.isNegated() ^ literal2.isNegated());

        if (differ) {

            Predicate p1 = literal1.getPredicate();
            Predicate p2 = literal2.getPredicate();

            return Unifier.unify(p1, p2);
        } else {
            return null;
        }

    }

    public List<List<Clause>> getMatchingClauses(Set<Clause> clauses, Problem problem, int size) {

        List<Set<Clause>> sets = newList();
        sets.add(clauses);
        sets.add(clauses);

        Set<List<Clause>> possiblePairs = cartesianProduct(sets);


        return possiblePairs.stream().filter(possiblePair -> {
            Clause left = possiblePair.get(0);
            Clause right = possiblePair.get(1);

            if (used.get(problem).contains(ImmutablePair.from(left, right)) || right.getLiterals().size()> size) {
                return false;
            }
            Set<Clause> resolvends = resolve(left, right);
            if (!resolvends.isEmpty()) {
                return true;
            } else {
                return false;
            }
        }).collect(Collectors.toList());


    }

    public Set<Clause> resolve(Clause clause1, Clause clause2) {

        Set<Literal> literals1 = clause1.getLiterals();
        Set<Literal> literals2 = clause2.getLiterals();

        List<Set<Literal>> pairs = newList();
        pairs.add(literals2);
        pairs.add(literals1);

        Set<List<Literal>> pairsSet = cartesianProduct(pairs);

        Set<Pair<List<Literal>, Map<Variable, Value>>> matches =
                pairsSet.stream().map(pair -> {
                    Literal left = pair.get(0);
                    Literal right = pair.get(1);

                    return ImmutablePair.from(pair, matches(left, right));
                }).filter(x -> x.second() != null).collect(Collectors.toSet());

        return matches.stream().map(match -> {
            Set<Literal> l1 = Sets.remove(literals1, match.first().get(0));
            Set<Literal> l2 = Sets.remove(literals2, match.first().get(1));

            Set<Literal> literals = Sets.union(l1, l2);
            return (new Clause(literals)).apply(match.second());

        }).collect(Collectors.toSet());


    }
}

