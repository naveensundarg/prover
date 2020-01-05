package com.naveensundarg.shadow.prover.core.propositionalmodalprovers;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.ccprovers.ModalConverter;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.core.proof.TrivialJustification;
import com.naveensundarg.shadow.prover.representations.cnf.PseudoLiteral;
import com.naveensundarg.shadow.prover.representations.formula.And;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Or;
import com.naveensundarg.shadow.prover.representations.formula.Possibility;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newEmptyList;
import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

public abstract class LP implements Prover {

    private Map<Formula, Set<Formula>> ancestors;
    @Override
    public synchronized Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        ancestors = CollectionUtils.newMap();
        Problem problem = new Problem("LP", "LP base prover", assumptions, formula);

        Set<Formula> base = CollectionUtils.newEmptySet();
        base.addAll(assumptions);
        base.add(Logic.negated(formula));
        applyLP1Rule(assumptions, base);


        Set<Formula> clausesR = base.
                stream().
                map(x -> {

                    Set<Formula> c = ModalConverter.convertToCNF(x, problem);

                    c.forEach(u -> ancestors.put(u, Sets.from(x)));
                    return c;
                }).
                reduce(Sets.newSet(), Sets::union);




        Pair<Optional<Justification>, Set<Formula>> startPair   = runResolutionTillEnd(clausesR);
        Set<Formula>                                currentBase = startPair.getRight();
        applyLP1Rule(assumptions, currentBase);

        Pair<Optional<Justification>, Set<Formula>> nextPair = runResolutionTillEnd(currentBase);

        while ((nextPair.getRight().size() != startPair.getRight().size()) && !nextPair.getLeft().isPresent()) {

            startPair = nextPair;
            currentBase = startPair.getRight();
            applyLP1Rule(assumptions, currentBase);
            nextPair = runResolutionTillEnd(startPair.getRight());
        }

        return nextPair.getLeft();
    }

    private void printAncestors(Formula f){

        if(ancestors.containsKey(f)){
            System.out.println(f + "<---- " + ancestors.get(f).stream().map(Object::toString).collect(Collectors.joining(" -- ")));

            ancestors.get(f).forEach(this::printAncestors);
        }

    }
    private void applyLP1Rule(Set<Formula> assumptions, Set<Formula> base) {
        Set<Or> disjunctions = base.stream().filter(f -> f instanceof Or).map(or -> (Or) or).collect(Collectors.toSet());

        disjunctions.forEach(disjunction -> {
            Arrays.stream(disjunction.getArguments()).filter(f -> this.canApplyRule(assumptions, f)).forEach(disjunct -> {
                Formula f = new Possibility(disjunct);
                 ancestors.put(f, Sets.from(disjunction));
                base.add(f);
            });
        });

        Set<Possibility> possibilities = base.stream().filter(f -> f instanceof Possibility).map(pos -> (Possibility) pos).collect(Collectors.toSet());


        possibilities.forEach(possibility1 -> {

            possibilities.forEach(possibility2 -> {

                if(!possibility1.equals(possibility2)){

                    Formula and = new And(possibility1.getFormula(), possibility2.getFormula());

                    if(canApplyRule(assumptions, and)){
                        Formula f = new Possibility(and);
                        ancestors.put(f, Sets.from(and));
                        f.setJustification(TrivialJustification.atomic("LP rule"));
                        base.add(f);
                    }
                }
            });
        });


    }


    private Pair<Optional<Justification>, Set<Formula>> runResolutionTillEnd(Set<Formula> clauses) {


        int k = getAgeWeightRatio();

        PriorityQueue<Formula> weightQueue = new PriorityQueue<>(Comparator.comparing(Formula::getWeight));
        Queue<Formula>         ageQueue    = new LinkedList<>();

        for (Formula clause : clauses) {
            weightQueue.add(clause);
            ageQueue.add(clause);
        }

        Set<Formula> usableList = CollectionUtils.newEmptySet();
        usableList.add(weightQueue.remove());

        int ageWeightCounter = 0;

        while (!weightQueue.isEmpty() && !ageQueue.isEmpty()) {

            Formula given = getNextFromStores(k, weightQueue, ageQueue, ageWeightCounter);
            ageWeightCounter = ageWeightCounter + 1;

            if (usableList.stream().anyMatch(x -> subsumes(x, given))) {
                continue;
            }
            for (Formula parent : usableList) {

                Set<Formula> resolvands = resolve(parent, given);

                for (Formula resolvand : resolvands) {

                    if (usableList.stream().anyMatch(x -> subsumes(x, resolvand))
                            || weightQueue.stream().anyMatch(x -> subsumes(x, resolvand))
                            || ageQueue.stream().anyMatch(x -> subsumes(x, resolvand))) {
                        continue;
                    }


                    if (resolvand.equals(Reader.FALSE) || (resolvand instanceof Or && ((Or) resolvand).getArguments().length == 0)) {

                        ancestors.put(Reader.FALSE, Sets.from(parent, given));

                        return org.apache.commons.lang3.tuple.ImmutablePair.of(
                                Optional.of(Justification.atomic("Proved")),
                                clauses);

                    } else {
                        ancestors.put(resolvand, Sets.from(parent, given));

                        if (!weightQueue.contains(resolvand) && !usableList.contains(resolvand)) {

                            weightQueue.add(resolvand);
                        }
                        if (!ageQueue.contains(resolvand) && !usableList.contains(resolvand)) {

                            ageQueue.add(resolvand);
                        }
                    }
                }


            }

            List<Formula> ageQueueProcessed = ageQueue.stream().filter(x -> !subsumes(given, x) && x.getWeight() <= getMaxWeight()).collect(Collectors.toList());
            ageQueue.clear();
            ageQueue.addAll(ageQueueProcessed);

            List<Formula> clauseStoreProcessed = weightQueue.stream().filter(x -> !subsumes(given, x) && x.getWeight() <= getMaxWeight()).collect(Collectors.toList());
            weightQueue.clear();
            weightQueue.addAll(clauseStoreProcessed);

            usableList = usableList.stream().filter(x -> !subsumes(given, x)).collect(Collectors.toSet());
            usableList.add(given);


        }

        return ImmutablePair.of(Optional.of(TrivialJustification.trivial(clauses, Logic.getFalseFormula())), Sets.union(Sets.union(clauses, weightQueue.stream().collect(Collectors.toSet())), ageQueue.stream().collect(Collectors.toSet())));
    }

    private Formula getNextFromStores(int k, PriorityQueue<Formula> weightQueue, Queue<Formula> ageQueue, int ageWeightCounter) {
        Formula given;
        if (ageWeightCounter % k == 0) {
            given = ageQueue.remove();
            weightQueue.remove(given);

        } else {
            given = weightQueue.remove();
            ageQueue.remove(given);
        }
        return given;
    }


    private Set<Formula> shadow(Set<Formula> formulas) {
        return formulas.stream().map(f -> f.shadow(1)).collect(Collectors.toSet());
    }

    private int getAgeWeightRatio() {
        return 5;
    }

    private int getMaxWeight() {
        return 60;
    }

    public boolean subsumes(Formula a, Formula b) {

        if (a instanceof Or) {
            if (b instanceof Or) {
                Or or1 = (Or) a;
                Or or2 = (Or) b;

                Set<Formula> disjuncts1 = Arrays.stream(or1.getArguments()).collect(Collectors.toSet());
                Set<Formula> disjuncts2 = Arrays.stream(or2.getArguments()).collect(Collectors.toSet());

                return disjuncts2.containsAll(disjuncts1);
            }
        }

        return a.equals(b);

    }


    public boolean matches(Formula formula1, Formula formula2) {

        PseudoLiteral literal1 = PseudoLiteral.from(formula1);
        PseudoLiteral literal2 = PseudoLiteral.from(formula2);

        return literal1.getFormula().equals(literal2.getFormula())
                && (literal1.isNegated() ^ literal2.isNegated());

    }


    public Set<Formula> resolve(Formula f1, Formula f2) {

        Or clause1, clause2;

        if (f1 instanceof Or) {
            clause1 = (Or) f1;
        } else {
            clause1 = new Or(f1);
        }


        if (f2 instanceof Or) {
            clause2 = (Or) f2;
        } else {
            clause2 = new Or(f2);
        }

        Set<Formula> literals1 = Arrays.stream(clause1.getArguments()).collect(Collectors.toSet());
        Set<Formula> literals2 = Arrays.stream(clause2.getArguments()).collect(Collectors.toSet());

        List<Set<Formula>> pairs = newEmptyList();
        pairs.add(literals2);
        pairs.add(literals1);

        Set<List<Formula>> pairsSet = cartesianProduct(pairs);

        Set<List<Formula>> matches =
                pairsSet.stream().filter(pair -> {
                    Formula left  = pair.get(0);
                    Formula right = pair.get(1);
                    return matches(left, right);
                }).collect(Collectors.toSet());

        return matches.stream().map(match -> {
            Set<Formula> l1 = Sets.remove(literals1, match.get(0));
            Set<Formula> l2 = Sets.remove(literals2, match.get(1));

            List<Formula> disjuncts = new ArrayList<>(Sets.union(l1, l2));

            if (disjuncts.isEmpty()) {
                return Reader.FALSE;
            }
            else {
                return new Or(disjuncts);
            }

        }).collect(Collectors.toSet());

    }


    public abstract boolean canApplyRule(Set<Formula> background, Formula f);
}
