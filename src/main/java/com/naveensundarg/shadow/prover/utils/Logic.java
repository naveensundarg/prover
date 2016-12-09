package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.SymbolGenerator;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;
import static com.naveensundarg.shadow.prover.utils.Sets.newSet;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Logic {

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


        return new Clause(clause.getLiterals().stream().map(literal ->
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
        Set<Formula> forms =  formula.subFormulae().stream().filter(x->!x.equals(formula)).collect(Collectors.toSet());;

        forms.forEach(x->agents.addAll(agentsInFormula(x)));


        return agents.stream().filter(x-> !(x instanceof Variable)).collect(Collectors.toSet());
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

        Set<Formula> forms =  formula.subFormulae().stream().filter(x->!x.equals(formula)).collect(Collectors.toSet());;

        forms.forEach(x->times.addAll(timesInFormula(x)));


        return times;
    }
    public static Set<Value> allAgents(Set<Formula> base) {

        Set<Value> agents = base.stream().map(Logic::agentsInFormula).reduce(CollectionUtils.newEmptySet(), CollectionUtils::union);

        if(agents.isEmpty()) {

            agents.add(I);
        }

        return agents;
    }

    public static Set<Value> allTimes(Set<Formula> base) {

        Set<Value> times = base.stream().map(Logic::timesInFormula).reduce(CollectionUtils.newEmptySet(), CollectionUtils::union);

        if(times.isEmpty()) {

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
                filter(f -> f instanceof  BaseFormula).
                map(f -> (BaseFormula) f).
                collect(Collectors.toSet());
    }

    public static Set<BaseFormula> baseFormulae(Set<Formula> base) {

        return base.stream().
                map(Logic::baseFormulae).
                reduce(Sets.newSet(), Sets::union);

    }
}
