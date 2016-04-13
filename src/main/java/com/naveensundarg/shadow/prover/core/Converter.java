package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.*;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.utils.Logic;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

/**
 * Created by naveensundarg on 4/12/16.
 */
public class Converter {



    public static CNFFormula convertToCNF(Formula formula){

        if(formula instanceof Atom){
            return new CNFFormula((Atom) formula);
        }

        if(formula instanceof Predicate){

            return new CNFFormula((Predicate) formula);
        }


        if(formula instanceof And){
            And and = (And) formula;

            Set<Clause> clauses = Arrays.stream(and.getArguments()).
                    map(Converter::convertToCNF).
                    map(CNFFormula::getClauses).
                    reduce(Sets.newSet(), Sets::union);

            return new CNFFormula(clauses);
        }

        if(formula instanceof Or){

            Or or = (Or) formula;

            List<Set<Clause>> clauses = Arrays.stream(or.getArguments()).
                    map(Converter::convertToCNF).
                    map(CNFFormula::getClauses).collect(Collectors.toList());


            Set<List<Clause>> prod = cartesianProduct(clauses);

            return new CNFFormula(prod.stream().map(Clause::fromClauses).collect(Collectors.toSet()));


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

        if(formula instanceof Universal){

            Universal universal = (Universal) formula;
            return convertToCNF(universal.getArgument());


        }
        if(isLiteral(formula)){
            Literal literal;
            if(formula instanceof Predicate){

                literal = new Literal((Predicate) formula, false);
            }
            else if(formula instanceof Not){
                Not not = (Not) formula;
                literal = new Literal((Predicate) not.getArgument(), true);

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

            if(notArg instanceof Existential){

                Existential existential = (Existential) notArg;


                Universal universal = new Universal(existential.vars(), Logic.negated(existential.getArgument()));

                return convertToCNF(universal);

            }

            if(notArg instanceof Universal){

                Universal universal = (Universal) notArg;


                Existential existential = new Existential(universal.vars(), Logic.negated(universal.getArgument()));

                return convertToCNF(existential);

            }


        }

        return null;

    }


    private static boolean isLiteral(Formula formula){

        return (formula instanceof Predicate) ||
                ((formula instanceof Not) &&
                        ((Not)  formula).getArgument() instanceof Predicate);
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


}
