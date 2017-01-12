package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.SymbolGenerator;
import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;
import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Logic {

    private Logic(){
        throw new AssertionError("CANNOT INSTANTIATE: " + Logic.class);
    }
    public static Formula negated(Formula f) {

        if (f instanceof Not) {

            return ((Not) f).getArgument();
        } else {
            return new Not(f);
        }
    }

    private static final Value I = new Constant("I");
    private static final Value now = new Constant("now");

    public static Clause renameVars(Clause clause, Problem problem) {

        Set<Variable> vars = clause.getLiterals().stream().map(Literal::getPredicate).map(Predicate::variablesPresent).reduce(newSet(), Sets::union);

        Map<Variable, Value> theta = newMap();

        vars.stream().forEach(variable -> theta.put(variable, SymbolGenerator.newVariable(problem)));


        return new Clause(false, clause.getLiterals().stream().map(literal ->
                new Literal((Predicate) literal.getPredicate().apply(theta), literal.isNegated())).
                collect(Collectors.toSet()));

    }

    private static Set<Value> agentsInFormula(Formula formula) {

        Set<Value> agents = CollectionUtils.newEmptySet();
        if (formula instanceof Belief) {
            agents.add(((Belief) formula).getAgent());
        }
        if (formula instanceof Knowledge) {
            agents.add(((Knowledge) formula).getAgent());
        }

        if (formula instanceof Ought) {
            agents.add(((Ought) formula).getAgent());
        }
        Set<Formula> forms = formula.subFormulae().stream().filter(x -> !x.equals(formula)).collect(Collectors.toSet());
        ;

        forms.forEach(x -> agents.addAll(agentsInFormula(x)));


        return agents.stream().filter(x -> !(x instanceof Variable)).collect(Collectors.toSet());
    }

    private static Set<Value> timesInFormula(Formula formula) {

        Set<Value> times = CollectionUtils.newEmptySet();
        if (formula instanceof Belief) {
            times.add(((Belief) formula).getTime());
        }
        if (formula instanceof Knowledge) {
            times.add(((Knowledge) formula).getTime());
        }

        if (formula instanceof Common) {
            times.add(((Common) formula).getTime());
        }

        if (formula instanceof Ought) {
            times.add(((Ought) formula).getTime());
        }

        Set<Formula> forms = formula.subFormulae().stream().filter(x -> !x.equals(formula)).collect(Collectors.toSet());
        ;

        forms.forEach(x -> times.addAll(timesInFormula(x)));


        return times;
    }

    public static Set<Value> allAgents(Set<Formula> base) {

        Set<Value> agents = base.stream().map(Logic::agentsInFormula).reduce(CollectionUtils.newEmptySet(), CollectionUtils::union);

        if (agents.isEmpty()) {

            agents.add(I);
        }

        return agents;
    }

    public static Set<Value> allTimes(Set<Formula> base) {

        Set<Value> times = base.stream().map(Logic::timesInFormula).reduce(CollectionUtils.newEmptySet(), CollectionUtils::union);

        if (times.isEmpty()) {

            times.add(now);
        }

        return times;
    }
/*
    public static Set<Predicate> variationsOf(Predicate predicate, Set<Formula> base) {

        return baseFormulae(base).stream().
                filter(p -> p.getName().equals(predicate.getName())).collect(Collectors.toSet());

    }*/

    public static Set<BaseFormula> baseFormulae(Formula formula) {

        return formula.subFormulae().stream().
                filter(f -> f instanceof BaseFormula).
                map(f -> (BaseFormula) f).
                collect(Collectors.toSet());
    }

    public static Set<BaseFormula> baseFormulae(Set<Formula> base) {

        return base.stream().
                map(Logic::baseFormulae).
                reduce(Sets.newSet(), Sets::union);

    }

    public static Map<Variable, Value> simplify(Set<Variable> variables) {

        List<Integer> ids = variables.stream().map(Variable::getId).collect(Collectors.toList());

        ids.sort(Integer::compareTo);

        int count = 0;

        Map<Variable, Value> answer = CollectionUtils.newMap();

        for (int x : ids) {

            answer.put(new Variable("?x" + x), new Variable("?x" + count));

            count++;

        }

        return answer;
    }


    public static boolean canKeepEquality(Literal literal) {


        boolean canRemove  = (literal.isNegated() &&
                literal.getPredicate().getName().equals("=") &&
                literal.getPredicate().getArguments().length == 2  &&
                literal.getPredicate().getArguments()[0].equals(literal.getPredicate().getArguments()[1]));




        return !canRemove;

    }


    public static Optional<Map<Variable, Value>> isInstantationOfQuantifier(Quantifier quantifier, Formula formula) {


        Variable variable = quantifier.vars()[0];
        Formula kernel;

        if(quantifier.vars().length>1){

            if(quantifier instanceof Universal){

                kernel = new Universal( Arrays.copyOfRange(quantifier.vars(), 1, quantifier.vars().length ), quantifier.getArgument());

            } else{

                kernel = new Existential( Arrays.copyOfRange(quantifier.vars(), 1, quantifier.vars().length ), quantifier.getArgument());

            }
        } else {

            kernel = quantifier.getArgument();;

        }


        Set<Variable> boundVarsInKernel = kernel.boundVariablesPresent();


        Optional<Map<Variable, Value>> substitutionsOpt = Unifier.unifyFormula(kernel, formula);

        if(!substitutionsOpt.isPresent()){
            return Optional.empty();
        }

        Map<Variable, Value> substitutions = substitutionsOpt.get();

        if(substitutions.size()>1){
            return Optional.empty();
        }

        if(substitutions.isEmpty()){

            return substitutionsOpt;
        }

        if(!substitutions.containsKey(variable)){

            return Optional.empty();
        }

        Value value = substitutions.get(variable);

        if(boundVarsInKernel.stream().anyMatch(value.subValues()::contains)){

            return Optional.empty();
        }

        return substitutionsOpt;



    }
    public static Clause simplifyWithEquality(Problem problem, Clause clause1, Clause clause2){


        if(!unitNonTrivialEquality(clause2) || unitTrivialEquality(clause1) || clause1.equals(clause2)){

            return clause1;

        }


        Literal equalityLiteral = clause2.getSortedLiterals().get(0);

        Value value1 = equalityLiteral.getPredicate().getArguments()[0];
        Value value2 = equalityLiteral.getPredicate().getArguments()[1];

        List<Literal> sortedLiterals = Logic.renameVars(clause1, problem).getSortedLiterals();

        Value biggerValue = value1.getWeight() >= value2.getWeight()? value1: value2;
        Value smallerValue = value1.getWeight() >= value2.getWeight()? value2: value1;

        if(!smallerValue.isConstant() || biggerValue.isVariable() || biggerValue.isConstant()){
            return clause1;
        }

        Set<Literal> newLiterals = Sets.newSet();
        boolean changed = false;
        for(Literal literal: sortedLiterals){

            Predicate predicate = literal.getPredicate();

            Value[] newValues = Arrays.copyOf(predicate.getArguments(), predicate.getArguments().length);
            for(int i =0; i< predicate.getArguments().length; i++){

                if(predicate.getArguments()[i].isVariable()){
                    Map<Variable, Value> mapping = Unifier.unify(biggerValue, predicate.getArguments()[i]);

                    if(mapping!=null){

                        changed = true;
                        newValues[i] = smallerValue.apply(mapping);

                    } else {

                        newValues[i] = predicate.getArguments()[i];
                    }
                }


            }

            Literal newLiteral = new Literal(new Predicate(predicate.getName(), newValues), literal.isNegated());

            newLiterals.add(newLiteral);

        }

        if(!changed){
            return clause1;
        } else{

            Clause newClause  = new Clause(newLiterals);

            return newClause;
        }


    }

    public static boolean unitTrivialEquality(Clause clause){

        if(clause.getLiterals().size()!=1){
            return false;
        }

        Literal literal = clause.getSortedLiterals().get(0);

        return  (!literal.isNegated() &&
                literal.getPredicate().getName().equals("=") &&
                literal.getPredicate().getArguments().length == 2  &&
                literal.getPredicate().getArguments()[0].equals(literal.getPredicate().getArguments()[1]));

    }
    public static boolean unitNonTrivialEquality(Clause clause){

        if(clause.getLiterals().size()!=1){
            return false;
        }

        Literal literal = clause.getSortedLiterals().get(0);

        return  (!literal.isNegated() &&
                literal.getPredicate().getName().equals("=") &&
                literal.getPredicate().getArguments().length == 2  &&
                !literal.getPredicate().getArguments()[0].equals(literal.getPredicate().getArguments()[1]));

    }

    public static boolean isTautology(Clause clause) {

        Set<Literal> literals = clause.getLiterals();

        return clause.getLiterals().stream().anyMatch(x -> literals.stream().anyMatch(y->x.getPredicate().equals(y.getPredicate()) && y.isNegated()!= x.isNegated()));

    }

    public static boolean isSyntacticallyCorrectEquality(Formula formula){

        return (formula instanceof Predicate) &&
                ((Predicate) formula).getName().equals("=") &&
                ((Predicate) formula).getArguments().length >= 2;


    }
}
