package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;
import static com.naveensundarg.shadow.prover.utils.Reader.readFormula;
import static com.naveensundarg.shadow.prover.utils.Reader.readFormulaFromString;
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

    private static Formula TRUE;
    private static Formula FALSE;
    private static Formula INCONSISTENT_FORMULA;

    public static Formula getTrueFormula() {
        return TRUE;
    }

    public static Formula getFalseFormula() {
        return FALSE;
    }

    public static Formula getInconsistentFormula() {
        return INCONSISTENT_FORMULA;
    }

    static {

        try {
            TRUE =  readFormulaFromString("True");
            FALSE =  readFormulaFromString("False");
            INCONSISTENT_FORMULA = readFormulaFromString("(and P (not P))");

        } catch (Reader.ParsingException e) {
            e.printStackTrace();
        }

    }



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
        if (formula instanceof Says) {
            agents.add(((Says) formula).getAgent());
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

        if (formula instanceof Says) {
            times.add(((Says) formula).getTime());
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

    public static boolean isConsistent(Set<Formula> formulae){


        return !(new SnarkWrapper()).prove(formulae, Logic.getInconsistentFormula()).isPresent();
    }

     public static boolean isConsistent(Prover prover, Set<Formula> formulae){


        return prover.prove(formulae, Logic.getInconsistentFormula()).isPresent();
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




   private static final AtomicInteger logicLevel = new AtomicInteger(1);

    public static final void setLevel(int level){

        logicLevel.set(level);
    }

    public static final int getLogicLevel(){

        return logicLevel.get();
    }

    private static final String holdsFOL = "@";
    public static Formula transformSecondOrderToFirstOrder(Formula formula){

        if(formula instanceof  Atom){
            Atom atom = (Atom) formula;
            return new Predicate(holdsFOL, new Value[]{new Constant(atom.getName())});
        }

        if(formula instanceof  Predicate){
            Predicate predicate = (Predicate) formula;
            Value[] args = new Value[predicate.getArguments().length + 1];
            args[0] = new Constant(predicate.getName());

            for(int i = 0; i< predicate.getArguments().length; i++){

                args[i+1] = predicate.getArguments()[i];
            }
            return new Predicate(holdsFOL, args);
        }

        if(formula instanceof Not){

            return new Not(transformSecondOrderToFirstOrder(((Not) formula).getArgument()));
        }

        if(formula instanceof Implication){

            return new Implication(
                    transformSecondOrderToFirstOrder(((Implication) formula).getAntecedent()),
                    transformSecondOrderToFirstOrder(((Implication) formula).getConsequent()));
        }
        if(formula instanceof And){

            And and = (And) formula;

            return new And(Arrays.stream(and.getArguments()).map(Logic::transformSecondOrderToFirstOrder).collect(Collectors.toList()));
        }

         if(formula instanceof Or){

            Or or = (Or) formula;

            return new Or(Arrays.stream(or.getArguments()).map(Logic::transformSecondOrderToFirstOrder).collect(Collectors.toList()));
        }

         if(formula instanceof Universal){

            Universal universal = (Universal) formula;

            return new Universal(universal.vars(), transformSecondOrderToFirstOrder(universal.getArgument()));
        }

        if(formula instanceof Existential){

            Existential existential = (Existential) formula;

            return new Existential(existential.vars(), transformSecondOrderToFirstOrder(existential.getArgument()));
        }

         if(formula instanceof Knowledge){

            Knowledge knowledge = (Knowledge) formula;

            return new Knowledge(knowledge.getAgent(), knowledge.getTime(), transformSecondOrderToFirstOrder(knowledge.getFormula()));
        }

        if(formula instanceof Belief){

            Belief belief = (Belief) formula;

            return new Belief(belief.getAgent(), belief.getTime(), transformSecondOrderToFirstOrder(belief.getFormula()));
        }

        if(formula instanceof Necessity){

            Necessity necessity = (Necessity) formula;

            return new Necessity(transformSecondOrderToFirstOrder(necessity.getFormula()));
        }

        if(formula instanceof Possibility){

            Possibility possibility = (Possibility) formula;

            return new Possibility(transformSecondOrderToFirstOrder(possibility.getFormula()));
        }


        throw new AssertionError("Cannot transform into FOL: " + formula);

    }

}
