package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.SymbolGenerator;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newEmptyList;
import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;
import static com.naveensundarg.shadow.prover.utils.Sets.cartesianProduct;

/**
 * Created by naveensundarg on 4/12/16.
 */
public class ColoredConverter {

    public static final Constant NONE = new Constant("NONE");
    public static final Constant NOT = new Constant("NOT");

    public static Value addToColor(Value a, Value color) {

        return new Compound("color", new Value[]{a, color});
    }


    public static CNFFormula convertToCNF(Formula formula, Problem problem) {

        return convertToCNFInternal(FolConverter.preProcess(formula, problem), newEmptyList(), problem, NONE);
    }


    private static CNFFormula convertToCNFInternal(Formula formula, List<Variable> variables, Problem problem, Value color) {

        if (formula instanceof Atom) {

            return new CNFFormula(augmentWithColor((Atom) formula, color));
        }

        if (formula instanceof Predicate) {

            return new CNFFormula(augmentWithColor((Predicate) formula, color));
        }


        if (formula instanceof And) {
            And and = (And) formula;

            Set<Clause> clauses = Arrays.stream(and.getArguments()).
                    map(x -> convertToCNFInternal(x, variables, problem, color)).
                    map(CNFFormula::getClauses).
                    reduce(Sets.newSet(), Sets::union);

            return new CNFFormula(clauses);
        }

        if (formula instanceof Or) {

            Or or = (Or) formula;

            List<Set<Clause>> clauses = Arrays.stream(or.getArguments()).
                    map(x -> convertToCNFInternal(x, variables, problem, color)).
                    map(CNFFormula::getClauses).collect(Collectors.toList());


            Set<List<Clause>> prod = cartesianProduct(clauses);

            return new CNFFormula(prod.stream().map(Clause::fromClauses).collect(Collectors.toSet()));


        }

        if (formula instanceof Implication) {

            Implication implication = (Implication) formula;

            Formula antecedent = implication.getAntecedent();
            Formula consequent = implication.getConsequent();

            return convertToCNFInternal(new Or(Logic.negated(antecedent), consequent), variables, problem, color);


        }

        if (formula instanceof BiConditional) {

            BiConditional biConditional = (BiConditional) formula;

            Formula left = biConditional.getLeft();
            Formula right = biConditional.getRight();

            return convertToCNFInternal(new And(new Implication(left, right), new Implication(right, left)), variables, problem, color);


        }

        if (formula instanceof Universal) {

            Universal universal = (Universal) formula;

            Set<Variable> topVars = variables.stream().collect(Collectors.toSet());

            Arrays.stream(((Universal) formula).vars()).forEach(topVars::add);

            if (universal.getArgument() instanceof And) {
                And and = (And) universal.getArgument();

                return convertToCNFInternal(new And(

                        Arrays.stream(and.getArguments()).map(conjunct -> {
                            Set<Variable> thisVars = Sets.intersection(topVars, conjunct.variablesPresent());

                            if (thisVars.isEmpty()) {
                                return conjunct;
                            } else {

                                Variable[] thisVarsArray = new Variable[thisVars.size()];
                                int count = 0;
                                for (Variable v : thisVars) {
                                    thisVarsArray[count] = v;
                                    count = count + 1;
                                }
                                return new Universal(thisVarsArray, conjunct);
                            }
                        }).collect(Collectors.toList())
                ), variables, problem, color);


            } else {

                return convertToCNFInternal(universal.getArgument(), topVars.stream().collect(Collectors.toList()), problem, color);

            }

        }

        if (formula instanceof Existential) {

            formula = skolemize(formula, variables, problem);
//            Existential existential = (Existential) formula;
            return convertToCNFInternal(formula, variables, problem, color);

        }
        if (isLiteral(formula)) {
            Literal literal;
            if (formula instanceof Predicate) {
                //TODO: Add color information here.
                literal = new Literal(augmentWithColor((Predicate)formula, color), false);
            } else if (formula instanceof Not) {
                Not not = (Not) formula;
                literal = new Literal(augmentWithColor((Predicate) not.getArgument(), color), true);

            } else {
                throw new AssertionError(formula);
            }
            return new CNFFormula(Sets.with(new Clause(false, Sets.with(literal))));
        }

        if (formula instanceof Not) {

            Not not = (Not) formula;
            Formula notArg = not.getArgument();


            if (notArg instanceof Not) {

                return convertToCNFInternal(((Not) notArg).getArgument(), variables, problem, color);
            }

            if (notArg instanceof Implication) {

                Implication implication = (Implication) notArg;

                And and = new And(implication.getAntecedent(),
                        Logic.negated(implication.getConsequent()));
                return convertToCNFInternal(and, variables, problem, color);
            }

            if (notArg instanceof BiConditional) {

                BiConditional biConditional = (BiConditional) notArg;

                Formula left = biConditional.getLeft();
                Formula right = biConditional.getRight();

                return convertToCNFInternal(new Or(new And(Logic.negated(left), right), new And(Logic.negated(right), left)), variables, problem, color);
            }
            if (notArg instanceof Or) {

                Or or = (Or) notArg;

                And and = new And(Arrays.stream(or.getArguments()).
                        map(Logic::negated).collect(Collectors.toList()));
                return convertToCNFInternal(and, variables, problem, color);
            }

            if (notArg instanceof And) {

                And and = (And) notArg;

                Or or = new Or(Arrays.stream(and.getArguments()).
                        map(Logic::negated).collect(Collectors.toList()));
                return convertToCNFInternal(or, variables, problem, color);
            }

            if (notArg instanceof Existential) {

                Existential existential = (Existential) notArg;


                Universal universal = new Universal(existential.vars(), Logic.negated(existential.getArgument()));

                return convertToCNFInternal(universal, variables, problem, color);

            }

            if (notArg instanceof Universal) {

                Universal universal = (Universal) notArg;


                Existential existential = new Existential(universal.vars(), Logic.negated(universal.getArgument()));

                return convertToCNFInternal(existential, variables, problem, color);

            }


        }


        if (formula instanceof Common) {

            Value thisColor = Color.getBaseColorTerm(new Constant("C"), ((Common) formula).getTime());

            return convertToCNFInternal(((Common) formula).getFormula(), variables, problem,  thisColor);


        }

        if (formula instanceof Belief) {

            Value thisColor = Color.getBaseColorTerm(new Constant("B"), ((Belief) formula).getAgent() , ((Belief) formula).getTime());

            return convertToCNFInternal(((Belief) formula).getFormula(), variables, problem,  Color.joinColor(color,thisColor));


        }

        if (formula instanceof Knowledge) {

            Value thisColor = Color.getBaseColorTerm(new Constant("K"), ((Knowledge) formula).getAgent() , ((Knowledge) formula).getTime());

            return convertToCNFInternal(((Knowledge) formula).getFormula(), variables, problem,  Color.joinColor(color,thisColor));


        }
        if (formula instanceof Perception) {

            Value thisColor = Color.getBaseColorTerm(new Constant("P"), ((Perception) formula).getAgent() , ((Perception) formula).getTime());

            return convertToCNFInternal(((Perception) formula).getFormula(), variables, problem,  thisColor);


        }

        return null;

    }

    private static boolean hasColor(Predicate predicate){

        return predicate.getArguments().length>=1 && predicate.getArguments()[predicate.getArguments().length-1].getName().equals("color");

    }

    static List<Value> getValues(Value color){
        if(color.equals(NONE)){
            return CollectionUtils.listOf(NONE);
        }
        else {
            List<Value> answer =  getValues(color.getArguments()[1]);
            answer.add(color.getArguments()[0]);
            return answer;
        }
    }

    static Value  makeColor(List<Value> color){

        if(color.isEmpty() || (color.size()==1 & color.get(0).equals(ColoredConverter.NONE))){
            return ColoredConverter.NONE;
        }
        else {

            return new Compound("color", new Value[]{color.get(0), makeColor(color.subList(1, color.size()))});
        }

    }
    private static Value joinColor(Value color1, Value color2){

        if(color1.equals(NONE)){
            return color2;
        }
        if(color2.equals(NONE)){
            return color1;
        }
        List<Value> colorValues1 = getValues(color1);
        List<Value> colorValues2 = getValues(color2);


        Value[] colors = new Value[colorValues1.size()+ colorValues1.size()];
        int i = 0;
        for(; i<colorValues1.size(); i++){
            colors[i]  = colorValues1.get(i);
        }

        for(; i<colorValues1.size(); i++){
            colors[i]  = colorValues2.get(i-colorValues1.size());
        }
        return Color.getBaseColorTerm(Arrays.copyOf(colorValues1.toArray(), colorValues1.size(), Value[].class));

    }
    public static Predicate augmentWithColor(Predicate formula, Value color) {
        if (color.equals(NONE)) {
            return  (formula);
        } else if (hasColor(formula)){

            Predicate predicate = formula;
            String predicateName = predicate.getName();
            Value[] args = predicate.getArguments();
            Value[] coloredArgs = Arrays.copyOf(args, args.length);

            coloredArgs[coloredArgs.length-1]= joinColor(color, coloredArgs[coloredArgs.length-1]);

            return (new Predicate(predicateName, coloredArgs));


        }else {
            Predicate predicate = formula;
            String predicateName = predicate.getName();
            Value[] args = predicate.getArguments();
            Value[] coloredArgs = Arrays.copyOf(args, args.length + 1);
            coloredArgs[coloredArgs.length-1] = color;
            return (new Predicate(predicateName, coloredArgs));
        }
    }


    public static Predicate augmentWithColor(Atom formula, Value color) {
        if (color.equals("NONE")) {
            return(formula);
        } else {
            return (new Predicate(formula.getName(), new Value[]{color}));

        }
    }


    public static Formula standardizeApart(Formula formula, Problem problem) {


        if (formula instanceof Universal) {

            Universal universal = (Universal) formula;

            Variable[] vars = universal.vars();

            List<Pair<Variable, Variable>> replacements = Arrays.stream(vars).
                    map(x -> ImmutablePair.from(x, SymbolGenerator.newVariable(problem))).collect(Collectors.toList());

            Map<Variable, Value> subs = newMap();

            replacements.forEach(pair -> subs.put(pair.first(), pair.second()));

            Formula argument = standardizeApart(universal.getArgument(), problem).apply(subs);

            Variable[] newVars = new Variable[vars.length];

            for (int i = 0; i < vars.length; i++) {

                newVars[i] = replacements.get(i).second();
            }


            return new Universal(newVars, argument);

        }

        if (formula instanceof Existential) {

            Existential existential = (Existential) formula;

            Variable[] vars = existential.vars();

            List<Pair<Variable, Variable>> replacements = Arrays.stream(vars).
                    map(x -> ImmutablePair.from(x, SymbolGenerator.newVariable(problem))).collect(Collectors.toList());

            Map<Variable, Value> subs = newMap();

            replacements.forEach(pair -> subs.put(pair.first(), pair.second()));


            Formula argument = standardizeApart(existential.getArgument(), problem).apply(subs);
            Variable[] newVars = new Variable[vars.length];

            for (int i = 0; i < vars.length; i++) {

                newVars[i] = replacements.get(i).second();
            }


            return new Existential(newVars, argument);

        }

        if (formula instanceof Not) {

            Not not = (Not) formula;
            return new Not(standardizeApart(not.getArgument(), problem));
        }

        if (formula instanceof Implication) {

            Implication implication = (Implication) formula;

            return new Implication(standardizeApart(implication.getAntecedent(), problem),
                    standardizeApart(implication.getConsequent(), problem));
        }

        if (formula instanceof BiConditional) {

            BiConditional biconditional = (BiConditional) formula;

            return new BiConditional(standardizeApart(biconditional.getLeft(), problem),
                    standardizeApart(biconditional.getRight(), problem));
        }


        if (formula instanceof And) {

            And and = (And) formula;

            return new And(Arrays.stream(and.getArguments()).map(x -> standardizeApart(x, problem)).collect(Collectors.toList()));
        }

        if (formula instanceof Or) {

            Or or = (Or) formula;

            return new Or(Arrays.stream(or.getArguments()).map(x -> standardizeApart(x, problem)).collect(Collectors.toList()));
        } else {

            return formula;
        }


    }


    public static Formula skolemize(Formula formula, Problem problem) {

        return skolemize(formula, newEmptyList(), problem);
    }


    private static Formula skolemize(Formula formula, List<Variable> variable, Problem problem) {

        if (formula instanceof Predicate) {
            return formula;
        }


        if (formula instanceof Not) {

            Not not = (Not) formula;
            return new Not(skolemize(not.getArgument(), variable, problem));
        }
        if (formula instanceof And) {

            And and = (And) formula;
            return new And(
                    Arrays.stream(and.getArguments()).
                            map(x -> skolemize(x, variable, problem)).collect(Collectors.toList()));
        }

        if (formula instanceof Or) {

            Or or = (Or) formula;
            return new Or(
                    Arrays.stream(or.getArguments()).
                            map(x -> skolemize(x, variable, problem)).collect(Collectors.toList()));
        }

        if (formula instanceof Implication) {

            Implication implication = (Implication) formula;

            return new Implication(skolemize(implication.getAntecedent(), variable, problem),
                    skolemize(implication.getConsequent(), variable, problem));
        }

        if (formula instanceof BiConditional) {

            BiConditional biConditional = (BiConditional) formula;

            return new BiConditional(skolemize(biConditional.getLeft(), variable, problem),
                    skolemize(biConditional.getRight(), variable, problem));
        }

        if (formula instanceof Universal) {

            Universal universal = (Universal) formula;

            List<Variable> added = newEmptyList();
            added.addAll(variable);
            added.addAll(Arrays.stream(universal.vars()).collect(Collectors.toList()));

            return new Universal(universal.vars(), skolemize(universal.getArgument(),
                    added, problem));
        }

        if (formula instanceof Existential) {

            Existential existential = (Existential) formula;

            Variable[] vars = existential.vars();

            //ToDo Skolemize
            List<Pair<Variable, Value>> replacements = Arrays.stream(vars).
                    map(x -> ImmutablePair.from(x, SymbolGenerator.skolem(variable, problem))).collect(Collectors.toList());

            Map<Variable, Value> subs = newMap();

            replacements.forEach(pair -> subs.put(pair.first(), pair.second()));


            return existential.getArgument().apply(subs);
        }


        return null;

    }

    public static boolean isLiteral(Formula formula) {

        return (formula instanceof Predicate) ||
                ((formula instanceof Not) &&
                        ((Not) formula).getArgument() instanceof Predicate);
    }

    private static boolean isDNF(Formula formula) {

        if (formula instanceof Or) {

            Or or = (Or) formula;

            return Arrays.stream(or.getArguments()).
                    allMatch(disjunct -> isLiteral(disjunct));


        } else {
            return false;
        }

    }


}
