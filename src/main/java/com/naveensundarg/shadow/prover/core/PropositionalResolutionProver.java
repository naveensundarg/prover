package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newEmptyList;
import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;
import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class PropositionalResolutionProver implements Prover {
    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        Set<CNFFormula> formulas = assumptions.stream().
                map(PropositionalResolutionProver::convertToCNF).
                collect(Collectors.toSet());


        formulas.add(convertToCNF(Logic.negated(formula)));

        Set<Clause> clauses = formulas.stream().
                map(CNFFormula::getClauses).
                reduce(newSet(),Sets::union);


        while(!clauses.isEmpty()){

            List<List<Clause>> matchingPairs = getMatchingClauses(clauses);

            if(matchingPairs.isEmpty()){
                return Optional.empty();
            }
            else{
                boolean expanded = false;
                for (List<Clause> pair : matchingPairs) {

                    Clause left = pair.get(0);
                    Clause right = pair.get(1);
                    boolean leftIsAllPositive = left.getLiterals().stream().noneMatch(Literal::isNegated);
                    boolean righIsAllPositive = right.getLiterals().stream().noneMatch(Literal::isNegated);


                    if (!leftIsAllPositive && !righIsAllPositive) {
                        continue;
                    }
                    Set<Clause> resolvands = resolve(left, right);


                    if (!resolvands.isEmpty()) {

                        List<Clause> resolvandsList = new ArrayList<>();
                        resolvands.forEach(resolvandsList::add);

                        for (Clause resolvand : resolvandsList) {
                            if (resolvand.getLiterals().isEmpty()) {
                                return Optional.of(Justification.trivial(assumptions, formula));
                            } else {
                                if (!clauses.contains(resolvand)) {
                                    clauses.add(resolvand);
                                    expanded = true;
                                }
                            }
                        }


                    } else {
                        return Optional.empty();
                    }
                }

                if(!expanded){
                    return Optional.empty();

                }


            }

        }

        return null;
    }

    public static CNFFormula convertToCNF(Formula formula){

        if(formula instanceof Atom){
            return new CNFFormula((Atom) formula);
        }

        if(formula instanceof And){
            And and = (And) formula;

            Set<Clause> clauses = Arrays.stream(and.getArguments()).
                    map(PropositionalResolutionProver::convertToCNF).
                    map(CNFFormula::getClauses).
                    reduce(Sets.newSet(), Sets::union);

           return new CNFFormula(clauses);
        }

        if(formula instanceof Or){

            Or or = (Or) formula;

            List<Set<Clause>> clauses = Arrays.stream(or.getArguments()).
                    map(PropositionalResolutionProver::convertToCNF).
                    map(CNFFormula::getClauses).collect(Collectors.toList());


            Set<List<Clause>> prod = cartesianProduct(clauses);

            return new CNFFormula(prod.stream().map(x->Clause.fromClauses(x)).collect(Collectors.toSet()));


        }

        if(formula instanceof Implication){

            Implication implication = (Implication) formula;

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            return convertToCNF(new Or(Logic.negated(antecedent),consequent));


        }

        if(formula instanceof BiConditional){

            BiConditional biConditional = (BiConditional) formula;

            Formula left = biConditional.getLeft();
            Formula right = biConditional.getRight();

            return convertToCNF(new And(new Implication(left, right), new Implication(right, left)));


        }
        if(isLiteral(formula)){
            Literal literal;
            if(formula instanceof Atom){

                literal = new Literal((Atom) formula, false);
            }
            else if(formula instanceof Not){
                Not not = (Not) formula;
                literal = new Literal((Atom) not.getArgument(), true);

            }
            else{
                throw new AssertionError(formula);
            }
            return new CNFFormula(Sets.with(new Clause(Sets.with(literal))));
        }

        if(formula instanceof Not){

            Not not = (Not) formula;
            Formula notArg =  not.getArgument();


            if(notArg instanceof Not){

                return convertToCNF(((Not)notArg).getArgument());
            }

            if(notArg instanceof Implication){

                Implication implication = (Implication) notArg;

                And and = new And(implication.getAntecedent(),
                        Logic.negated(implication.getConsequent()));
                return convertToCNF(and);
            }

            if(notArg instanceof BiConditional){

                BiConditional biConditional = (BiConditional) notArg;

                Formula left = biConditional.getLeft();
                Formula right = biConditional.getRight();

                return convertToCNF(new Or(new And(Logic.negated(left), right), new And(Logic.negated(right), left)));
            }
            if(notArg instanceof Or){

                Or or = (Or) notArg;

                And and = new And(Arrays.stream(or.getArguments()).
                        map(Logic::negated).collect(Collectors.toList()));
                return convertToCNF(and);
            }

            if(notArg instanceof And){

                And and = (And) notArg;

                Or or = new Or(Arrays.stream(and.getArguments()).
                        map(Logic::negated).collect(Collectors.toList()));
                return convertToCNF(or);
            }
        }

        return null;

    }


    private static boolean isLiteral(Formula formula){

        return (formula instanceof Atom) ||
                ((formula instanceof Not) &&
                        ((Not)  formula).getArgument() instanceof Atom);
    }
    private static boolean isDNF(Formula formula){

        if(formula instanceof Or){

            Or or = (Or) formula;

            return Arrays.stream(or.getArguments()).
                    allMatch(disjunct -> isLiteral(disjunct));


        } else{
            return false;
        }

    }


    public boolean matches(Literal literal1, Literal literal2){

        return  literal1.getPredicate().equals(literal2.getPredicate())
                && (literal1.isNegated() ^ literal2.isNegated());

    }

    public List<List<Clause>> getMatchingClauses(Set<Clause> clauses){

        List<Set<Clause>> sets = newEmptyList();
        sets.add(clauses);
        sets.add(clauses);

        Set<List<Clause>> possiblePairs = cartesianProduct(sets);

       return possiblePairs.stream().filter(possiblePair->{
            Clause left = possiblePair.get(0);
            Clause right = possiblePair.get(1);
            Set<Clause> resolvends = resolve(left, right);
           return !resolvends.isEmpty();
        }).collect(Collectors.toList());


    }

    public Set<Clause> resolve(Clause clause1, Clause clause2){

        Set<Literal> literals1 = clause1.getLiterals();
        Set<Literal> literals2 = clause2.getLiterals();

        List<Set<Literal>> pairs = newEmptyList();
        pairs.add(literals2);
        pairs.add(literals1);

        Set<List<Literal>> pairsSet = cartesianProduct(pairs);

        Set<List<Literal>> matches =

                pairsSet.stream().filter(pair->{
            Literal left = pair.get(0);
            Literal right = pair.get(1);

            return matches(left, right);
        }).collect(Collectors.toSet());

        return matches.stream().map(match->{
            Set<Literal> l1 = Sets.remove(literals1, match.get(0));
            Set<Literal> l2 = Sets.remove(literals2, match.get(1));

            Set<Literal> literals = Sets.union(l1, l2);
            return new Clause(literals);

        }).collect(Collectors.toSet());


    }

}
