package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.*;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.utils.ImmutablePair;
import com.naveensundarg.shadow.prover.utils.Logic;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newList;
import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;
import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

/**
 * Created by naveensundarg on 4/12/16.
 */
public class Converter {


    public static CNFFormula convertToCNF(Formula formula, Problem problem) {

        return convertToCNFInternal(standardizeApart(formula,problem), problem);
    }


        private static CNFFormula convertToCNFInternal(Formula formula, Problem problem){

        if(formula instanceof Atom){
            return new CNFFormula((Atom) formula);
        }

        if(formula instanceof Predicate){

            return new CNFFormula((Predicate) formula);
        }


        if(formula instanceof And){
            And and = (And) formula;

            Set<Clause> clauses = Arrays.stream(and.getArguments()).
                    map(x->convertToCNF(x, problem)).
                    map(CNFFormula::getClauses).
                    reduce(Sets.newSet(), Sets::union);

            return new CNFFormula(clauses);
        }

        if(formula instanceof Or){

            Or or = (Or) formula;

            List<Set<Clause>> clauses = Arrays.stream(or.getArguments()).
                    map(x->convertToCNF(x, problem)).
                    map(CNFFormula::getClauses).collect(Collectors.toList());


            Set<List<Clause>> prod = cartesianProduct(clauses);

            return new CNFFormula(prod.stream().map(Clause::fromClauses).collect(Collectors.toSet()));


        }

        if(formula instanceof Implication){

            Implication implication = (Implication) formula;

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            return convertToCNF(new Or(Logic.negated(antecedent),consequent), problem);


        }

        if(formula instanceof BiConditional){

            BiConditional biConditional = (BiConditional) formula;

            Formula left = biConditional.getLeft();
            Formula right = biConditional.getRight();

            return convertToCNF(new And(new Implication(left, right), new Implication(right, left)), problem);


        }

        if(formula instanceof Universal){

            Universal universal = (Universal) formula;
            return convertToCNF(universal.getArgument(), problem);

        }

        if(formula instanceof Existential){

            formula = skolemize(formula, problem);
//            Existential existential = (Existential) formula;
            return convertToCNF(formula, problem);

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

                return convertToCNF(((Not)notArg).getArgument(), problem);
            }

            if(notArg instanceof Implication){

                Implication implication = (Implication) notArg;

                And and = new And(implication.getAntecedent(),
                        Logic.negated(implication.getConsequent()));
                return convertToCNF(and, problem);
            }

            if(notArg instanceof BiConditional){

                BiConditional biConditional = (BiConditional) notArg;

                Formula left = biConditional.getLeft();
                Formula right = biConditional.getRight();

                return convertToCNF(new Or(new And(Logic.negated(left), right), new And(Logic.negated(right), left)), problem);
            }
            if(notArg instanceof Or){

                Or or = (Or) notArg;

                And and = new And(Arrays.stream(or.getArguments()).
                        map(Logic::negated).collect(Collectors.toList()));
                return convertToCNF(and, problem);
            }

            if(notArg instanceof And){

                And and = (And) notArg;

                Or or = new Or(Arrays.stream(and.getArguments()).
                        map(Logic::negated).collect(Collectors.toList()));
                return convertToCNF(or, problem);
            }

            if(notArg instanceof Existential){

                Existential existential = (Existential) notArg;


                Universal universal = new Universal(existential.vars(), Logic.negated(existential.getArgument()));

                return convertToCNF(universal, problem);

            }

            if(notArg instanceof Universal){

                Universal universal = (Universal) notArg;


                Existential existential = new Existential(universal.vars(), Logic.negated(universal.getArgument()));

                return convertToCNF(existential, problem);

            }


        }

        return null;

    }


    private static Formula standardizeApart(Formula formula, Problem problem){


        if(formula instanceof Universal){

            Universal universal = (Universal) formula;

            Variable[] vars = universal.vars();

            List<Pair<Variable,Variable>> replacements = Arrays.stream(vars).
                    map(x-> ImmutablePair.from(x, SymbolGenerator.newVariable(problem))).collect(Collectors.toList());

            Map<Variable, Value> subs = newMap();

            replacements.forEach(pair-> subs.put(pair.first(), pair.second()));

            Formula argument = standardizeApart(universal.getArgument(), problem).apply(subs);

            Variable[] newVars = new Variable[vars.length];

            for(int i = 0; i<vars.length; i++){

                newVars[i] = replacements.get(i).second();
            }


            return new Universal(newVars, argument);

        }

        if(formula instanceof Existential){

            Existential existential = (Existential) formula;

            Variable[] vars = existential.vars();

            List<Pair<Variable,Variable>> replacements = Arrays.stream(vars).
                    map(x-> ImmutablePair.from(x, SymbolGenerator.newVariable(problem))).collect(Collectors.toList());

            Map<Variable, Value> subs = newMap();

            replacements.forEach(pair-> subs.put(pair.first(), pair.second()));



            Formula argument = standardizeApart(existential.getArgument(), problem).apply(subs);
            Variable[] newVars = new Variable[vars.length];

            for(int i = 0; i<vars.length; i++){

                newVars[i] = replacements.get(i).second();
            }


            return new Existential(newVars, argument);

        }

        if(formula instanceof Not){

            Not not = (Not) formula;
            return new Not(standardizeApart(not.getArgument(),problem));
        }

        if(formula instanceof Implication){

            Implication implication = (Implication) formula;

            return  new Implication(standardizeApart(implication.getAntecedent(), problem),
                    standardizeApart(implication.getConsequent(), problem));
        }

        if(formula instanceof BiConditional){

            BiConditional biconditional = (BiConditional) formula;

            return  new BiConditional(standardizeApart(biconditional.getLeft(), problem),
                    standardizeApart(biconditional.getRight(), problem));
        }


        if(formula instanceof And){

            And and = (And) formula;

            return  new And(Arrays.stream(and.getArguments()).map(x->standardizeApart(x, problem)).collect(Collectors.toList()));
        }

        if(formula instanceof Or){

            Or or = (Or) formula;

            return  new Or(Arrays.stream(or.getArguments()).map(x->standardizeApart(x, problem)).collect(Collectors.toList()));
        }

        else {

            return formula;
        }


    }


    public static Formula skolemize(Formula formula, Problem problem){

        return skolemize(formula,  newList(), problem);
    }




    private static Formula skolemize(Formula formula, List<Variable> variable, Problem problem){

        if(formula instanceof Predicate){
            return formula;
        }


        if(formula instanceof Not){

            Not not = (Not) formula;
            return new Not(skolemize(not.getArgument(), variable, problem));
        }
        if(formula instanceof And){

            And and = (And) formula;
            return new And(
                    Arrays.stream(and.getArguments()).
                            map(x->skolemize(x, variable, problem)).collect(Collectors.toList()));
        }

        if(formula instanceof Or){

            Or or = (Or) formula;
            return new Or(
                    Arrays.stream(or.getArguments()).
                            map(x->skolemize(x, variable, problem)).collect(Collectors.toList()));
        }

        if(formula instanceof Implication){

            Implication implication = (Implication) formula;

            return new Implication(skolemize(implication.getAntecedent(), variable, problem),
                    skolemize(implication.getConsequent(), variable, problem));
        }

        if(formula instanceof BiConditional){

            BiConditional biConditional = (BiConditional) formula;

            return new BiConditional(skolemize(biConditional.getLeft(), variable, problem),
                    skolemize(biConditional.getRight(), variable, problem));
        }

        if(formula instanceof Universal){

            Universal universal = (Universal) formula;

            List<Variable> added = newList();
            added.addAll(variable);
            added.addAll(Arrays.stream(universal.vars()).collect(Collectors.toList()));

            return new Universal(universal.vars(), skolemize(universal.getArgument(),
                    added,   problem));
        }

        if(formula instanceof Existential){

            Existential existential = (Existential) formula;

            Variable[] vars = existential.vars();

            //ToDo Skolemize
            List<Pair<Variable,Value>> replacements = Arrays.stream(vars).
                    map(x-> ImmutablePair.from(x, (Value)SymbolGenerator.newConstant(problem))).collect(Collectors.toList());

            Map<Variable, Value> subs = newMap();

            replacements.forEach(pair-> subs.put(pair.first(), pair.second()));



            return existential.getArgument().apply(subs);
        }


        return  null;

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
