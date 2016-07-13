package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.core.Problem;
import com.naveensundarg.shadow.prover.core.SymbolGenerator;
import com.naveensundarg.shadow.prover.representations.*;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;
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

    private static final  Value I = new Constant("I");

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

        Set<Formula> forms =  formula.subFormulae().stream().filter(x->!x.equals(formula)).collect(Collectors.toSet());;

        forms.forEach(x->agents.addAll(agentsInFormula(x)));


        return agents;
    }

    public static Set<Value> allAgents(Set<Formula> base) {

        Set<Value> agents = base.stream().map(Logic::agentsInFormula).reduce(CollectionUtils.newEmptySet(), CollectionUtils::union);
        agents.add(I);

        return agents;
    }


}
