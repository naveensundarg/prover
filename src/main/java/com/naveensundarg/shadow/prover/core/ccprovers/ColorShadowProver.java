package com.naveensundarg.shadow.prover.core.ccprovers;

import com.naveensundarg.shadow.prover.core.*;
import com.naveensundarg.shadow.prover.core.proof.Justification;
import com.naveensundarg.shadow.prover.representations.cnf.CNFFormula;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Problem;
import com.naveensundarg.shadow.prover.utils.Sets;
import org.apache.commons.lang3.tuple.Pair;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 5/15/17.
 */
public class ColorShadowProver implements Prover {

    @FunctionalInterface
    interface PreProcessor{

        Formula process(Formula f);
    }

    private static Prover folProver = SnarkWrapper.getInstance();


    PreProcessor knowledgeLifter = f -> {

        return  f;

    };

    private static Formula constructCommonToKnowledge(Predicate predicate) {


        int n = Math.max(1, predicate.getArguments().length);

        Value[] argsC = new Value[n];
        Value[] argsK = new Value[n];
        Variable[] vars = new Variable[n + 2];
        for (int i = 0; i < n - 1; i++) {
            vars[i] = new Variable("?x" + i);

            argsC[i] = vars[i];
            argsK[i] = vars[i];
        }

        argsC[n - 1] =
                new Compound("color", new Value[]{new Constant("C"),
                        new Compound("color", new Value[]{new Variable("?t"),
                                new Variable("?x")})});

        argsK[n - 1] =
                new Compound("color", new Value[]{new Constant("K"),
                        new Compound("color", new Value[]{new Variable("?agent"),
                                new Compound("color", new Value[]{new Variable("?t"), new Variable("?x")})})});

        vars[n - 1] = new Variable("?x");
        vars[n] = new Variable("?agent");
        vars[n + 1] = new Variable("?time");

        Predicate predicateC = new Predicate(predicate.getName(), argsC);
        Predicate predicateK = new Predicate(predicate.getName(), argsK);

        return new Universal(vars,
                new Implication(predicateC, predicateK));

    }

    private static Formula constructKnowledgetoBeliefAxiom(Predicate predicate) {


        int n = Math.max(1, predicate.getArguments().length);

        Value[] argsK = new Value[n];
        Value[] argsB = new Value[n];
        Variable[] vars = new Variable[n];
        for (int i = 0; i < n - 1; i++) {
            vars[i] = new Variable("?x" + i);

            argsK[i] = vars[i];
            argsB[i] = vars[i];
        }

        argsK[n - 1] = new Compound("color", new Value[]{new Constant("K"), new Variable("?x")});
        argsB[n - 1] = new Compound("color", new Value[]{new Constant("B"), new Variable("?x")});
        vars[n - 1] = new Variable("?x");
        Predicate predicateK = new Predicate(predicate.getName(), argsK);
        Predicate predicateB = new Predicate(predicate.getName(), argsB);

        return new Universal(vars,
                new Implication(predicateK, predicateB));

    }

    private static Formula constructKnowledgetoProposition(Predicate predicate) {


        int n = Math.max(1, predicate.getArguments().length);
        Value[] argsK = new Value[n];
        Value[] argsProp = new Value[n];
        Variable[] vars = new Variable[n + 2];
        for (int i = 0; i < n - 1; i++) {
            vars[i] = new Variable("?x" + i);

            argsK[i] = vars[i];
            argsProp[i] = vars[i];
        }

        if (n == 0) {
            int x = 1;
        }
        argsK[n - 1] = new Compound("color",
                new Value[]{new Constant("K"),
                        new Compound("color", new Value[]{new Variable("?agent"),
                                new Compound("color", new Value[]{new Variable("?time"), new Variable("?x")})})});
        argsProp[n - 1] = new Variable("?x");
        vars[n - 1] = new Variable("?x");
        vars[n] = new Variable("?agent");
        vars[n + 1] = new Variable("?time");
        Predicate predicateK = new Predicate(predicate.getName(), argsK);
        Predicate predicateB = new Predicate(predicate.getName(), argsProp);

        return new Universal(vars,
                new Implication(predicateK, predicateB));

    }

    private static Formula constructPerceptiontoKnowledge(Predicate predicate) {


        int n = Math.max(1, predicate.getArguments().length);
        Value[] argsP = new Value[n];
        Value[] argsK = new Value[n];
        Variable[] vars = new Variable[n];
        for (int i = 0; i < n - 1; i++) {
            vars[i] = new Variable("?x" + i);

            argsP[i] = vars[i];
            argsK[i] = vars[i];
        }

        argsP[n - 1] = new Compound("color", new Value[]{new Constant("P"), new Variable("?x")});
        argsK[n - 1] = new Compound("color", new Value[]{new Constant("K"), new Variable("?x")});
        vars[n - 1] = new Variable("?x");
        Predicate predicateP = new Predicate(predicate.getName(), argsP);
        Predicate predicateK = new Predicate(predicate.getName(), argsK);

        return new Universal(vars,
                new Implication(predicateP, predicateK));

    }

    @Override
    public Optional<Justification> prove(Set<Formula> assumptions, Formula formula) {

        Problem problem = new Problem("", "", assumptions, formula);

        Set<Formula> coloredAssumptions = assumptions.stream().
                map(f -> ColoredConverter.convertToCNF(f, problem)).
                map(CNFFormula::toFormula)
                .collect(Collectors.toSet());


        Formula coloredGoal = ColoredConverter.convertToCNF(formula, problem).toFormula();

        addColorAssumptions(problem, coloredAssumptions, coloredGoal);


        return folProver.prove(coloredAssumptions, coloredGoal);

    }

    public Optional<Value> proveAndGetBinding(Set<Formula> assumptions, Formula formula, Variable variable){

        Problem problem = new Problem("", "", assumptions, formula);

        Set<Formula> coloredAssumptions = assumptions.stream().
                map(f -> ColoredConverter.convertToCNF(f, problem)).
                map(CNFFormula::toFormula)
                .collect(Collectors.toSet());


        Formula coloredGoal = ColoredConverter.convertToCNF(formula, problem).toFormula();

        addColorAssumptions(problem, coloredAssumptions, coloredGoal);

        return folProver.proveAndGetBinding(coloredAssumptions, coloredGoal, variable);
    }


    public Optional<Map<Variable, Value>> proveAndGetBindings(Set<Formula> assumptions, Formula formula, List<Variable> variables){

        Problem problem = new Problem("", "", assumptions, formula);

        Set<Formula> coloredAssumptions = assumptions.stream().
                map(f -> ColoredConverter.convertToCNF(f, problem)).
                map(CNFFormula::toFormula)
                .collect(Collectors.toSet());


        Formula coloredGoal = ColoredConverter.convertToCNF(formula, problem).toFormula();

        addColorAssumptions(problem, coloredAssumptions, coloredGoal);

        return folProver.proveAndGetBindings(coloredAssumptions, coloredGoal, variables);

    }


    public Optional<Pair<Justification, Set<Map<Variable, Value>>>> proveAndGetMultipleBindings(Set<Formula> assumptions, Formula formula, List<Variable> variables){

                Problem problem = new Problem("", "", assumptions, formula);

        Set<Formula> coloredAssumptions = assumptions.stream().
                map(f -> ColoredConverter.convertToCNF(f, problem)).
                map(CNFFormula::toFormula)
                .collect(Collectors.toSet());


        Formula coloredGoal = ColoredConverter.convertToCNF(formula, problem).toFormula();

        addColorAssumptions(problem, coloredAssumptions, coloredGoal);

        return folProver.proveAndGetMultipleBindings(coloredAssumptions, coloredGoal, variables);

    }

    private void addColorAssumptions(Problem problem, Set<Formula> coloredAssumptions, Formula coloredGoal) {
        Set<Formula> KtoBaxioms = Sets.add(coloredAssumptions, coloredGoal).stream()
                .flatMap(x -> x.subFormulae().stream())
                .filter(x -> x instanceof Predicate)
                .map(x -> (Predicate) x)
                .map(ColorShadowProver::constructKnowledgetoBeliefAxiom)
                .map(f -> ColoredConverter.convertToCNF(f, problem))
                .map(CNFFormula::toFormula)
                .collect(Collectors.toSet());

        Set<Formula> KtoPropaxioms = Sets.add(coloredAssumptions, coloredGoal).stream()
                .flatMap(x -> x.subFormulae().stream())
                .filter(x -> x instanceof Predicate)
                .map(x -> (Predicate) x)
                .map(ColorShadowProver::constructKnowledgetoProposition)
                .map(f -> ColoredConverter.convertToCNF(f, problem))
                .map(CNFFormula::toFormula)
                .collect(Collectors.toSet());

        Set<Formula> PtoKaxioms = Sets.add(coloredAssumptions, coloredGoal).stream()
                .flatMap(x -> x.subFormulae().stream())
                .filter(x -> x instanceof Predicate)
                .map(x -> (Predicate) x)
                .map(ColorShadowProver::constructPerceptiontoKnowledge)
                .map(f -> ColoredConverter.convertToCNF(f, problem))
                .map(CNFFormula::toFormula)
                .collect(Collectors.toSet());


        Set<Formula> CtoKAxioms = Sets.add(coloredAssumptions, coloredGoal).stream()
                .flatMap(x -> x.subFormulae().stream())
                .filter(x -> x instanceof Predicate)
                .map(x -> (Predicate) x)
                .map(ColorShadowProver::constructCommonToKnowledge)
                .map(f -> ColoredConverter.convertToCNF(f, problem))
                .map(CNFFormula::toFormula)
                .collect(Collectors.toSet());


      //  coloredAssumptions.addAll(coloredAssumptions.stream().map(knowledgeLifter::process).collect(Collectors.toSet()));

        coloredAssumptions.addAll(KtoBaxioms);
        coloredAssumptions.addAll(KtoPropaxioms);
        coloredAssumptions.addAll(PtoKaxioms);
        coloredAssumptions.addAll(CtoKAxioms);
    }




}
