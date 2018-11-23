package com.naveensundarg.shadow.prover.generators;


import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.AtomicJustification;
import com.naveensundarg.shadow.prover.core.proof.CompoundJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import javax.swing.text.html.Option;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class FOLPropositionalProblemGenerator {

    //static Prover prover = SnarkWrapper.getInstance();

    private final int maxAtoms;
    private final int totalClauses;
    private final int maxArity;
    private final int maxTermDepth;
    private final int maxFormulaDepth;
    private final int maxInferenceDepth;

    public static void main(String[] args) {

        GeneratorParams generatorParams = new GeneratorParams();

        generatorParams.maxAtoms = 5;
        generatorParams.maxLiteralsInClause = 5;
        generatorParams.clauses = 5;
        generatorParams.maxArity = 3;
        generatorParams.maxTermDepth = 2;
        generatorParams.maxFormulaDepth = 2;
        generatorParams.maxInferenceDepth = 20;
        FOLPropositionalProblemGenerator folPropositionalProblemGenerator = new FOLPropositionalProblemGenerator(generatorParams);

        Pair<Set<Formula>, Formula> problem = folPropositionalProblemGenerator.generateProblem();

        problem.first().forEach(System.out::println);

        System.out.println("------");

        System.out.println(problem.second());
        System.out.println(problem.second().getWeight());

    }

    private FOLPropositionalProblemGenerator(GeneratorParams generatorParams) {

        this.maxAtoms = generatorParams.maxAtoms;
        int maxLiteralsInClause = generatorParams.maxLiteralsInClause;
        this.totalClauses = generatorParams.clauses;
        this.maxArity = generatorParams.maxArity;
        this.maxTermDepth = generatorParams.maxTermDepth;
        this.maxFormulaDepth = generatorParams.maxFormulaDepth;
        this.maxInferenceDepth = generatorParams.maxInferenceDepth;
    }


    static {

    }


    private Pair<Set<Formula>, Formula> generateProblem() {


        Set<Formula> formulae = CollectionUtils.newEmptySet();

        for (int i = 0; i < 5; i++) {

            Formula given = (generateRandomFormula(0, CollectionUtils.newEmptyList()));
            given.setJustification(new AtomicJustification("GIVEN"));
            formulae.add(given);

        }

        Set<Formula> assumptionBase = CollectionUtils.setFrom(formulae);

        Optional<Formula> formulaOptional = randomlyInfer(formulae, maxFormulaDepth, 0);
        formulaOptional.ifPresent(assumptionBase::add);

        Formula f = null;

        if (formulaOptional.isPresent()) {

            f = formulaOptional.get();
        }

        for (int i = 0; i < maxInferenceDepth + 1; i++) {

            formulaOptional = randomlyInfer(assumptionBase, maxFormulaDepth, 0);

            formulaOptional.ifPresent(assumptionBase::add);

            if (formulaOptional.isPresent()) {

                f = formulaOptional.get();

            }

        }


        Set<Formula> usable = assumptionBase.stream().filter(x -> !(x instanceof Or) && !(x instanceof And)).collect(Collectors.toSet());
        ArrayList<Formula> dervied = new ArrayList<>(Sets.difference(usable, formulae));
        return ImmutablePair.from(formulae, dervied.get(ThreadLocalRandom.current().nextInt(dervied.size())));

    }


    private Constant generateRandomConstant() {

        return new Constant(Names.CONSTANTS[ThreadLocalRandom.current().nextInt(0, Names.CONSTANTS.length)]);

    }

    private Variable generateRandomVariable() {

        return new Variable(Names.VARIABLES[ThreadLocalRandom.current().nextInt(Names.VARIABLES.length)]);

    }

    private Variable generateRandomVariable(Set<Variable> exclusions) {

        List<String> scope = new ArrayList<>(Sets.difference(Arrays.stream(Names.VARIABLES).collect(Collectors.toSet()), exclusions.stream().map(Variable::toString).collect(Collectors.toSet())));


        return new Variable(scope.get(ThreadLocalRandom.current().nextInt(scope.size())));

    }

    private Value generateRandomGroundValue(int depth) {

        if (depth > maxTermDepth || ThreadLocalRandom.current().nextBoolean()) {


            return generateRandomConstant();

        } else {

            String compoundName = (Names.UNARY_FUNCTIONS[ThreadLocalRandom.current().nextInt(0, Names.UNARY_FUNCTIONS.length)]);


            Value[] arguments = new Value[1]; //For now just 1.

            for (int i = 0; i < arguments.length; i++) {

                //TODO: Make this tunable
                if (ThreadLocalRandom.current().nextBoolean()) {

                    arguments[i] = generateRandomGroundValue(depth + 1);

                } else {

                    arguments[i] = generateRandomConstant();

                }


            }

            return new Compound(compoundName, arguments);
        }


    }


    private Formula generateRandomFormula(int depth) {

        return generateRandomFormula(depth, CollectionUtils.newEmptyList());
    }

    private Formula generateRandomFormula(int depth, List<Variable> variables) {


        if (depth > maxFormulaDepth || ThreadLocalRandom.current().nextBoolean()) {

            Formula f = generateRandomPredicate(variables);
            f.setAssumptions(Sets.with(f));

            return f;
        } else {


            Supplier generateAnd = () -> {

                Formula f1  = generateRandomFormula(depth + 1, variables);
                Formula f2  = generateRandomFormula(depth + 1, variables);
                Formula and = new And(f1, f2);
                and.setJustification(new CompoundJustification("and-intro", CollectionUtils.listOf(f1.getJustification(), f2.getJustification())));
                return and;
            };

            Supplier generateOr     = () -> new Or(generateRandomFormula(depth + 1, variables), generateRandomFormula(depth + 1, variables));
            Supplier generateIf     = () -> new Implication(generateRandomFormula(depth + 1, variables), generateRandomFormula(depth + 1, variables));
            Supplier generateIff    = () -> new BiConditional(generateRandomFormula(depth + 1, variables), generateRandomFormula(depth + 1, variables));
            Supplier generateNot    = () -> new Not(generateRandomFormula(depth + 1, variables));
            Supplier generateEquals = () -> new Predicate("=", new Value[]{generateRandomGroundValue(0), generateRandomGroundValue(0)});


            Supplier generateForAll = () -> {

                Variable var = generateRandomVariable(new HashSet<>(variables));
                return new Universal(new Variable[]{var}, generateRandomFormula(depth + 1, CollectionUtils.addToList(variables, var)));
            };

            Supplier generateExists = () -> {

                Variable var = generateRandomVariable(new HashSet<>(variables));
                return new Existential(new Variable[]{var}, generateRandomFormula(depth + 1, CollectionUtils.addToList(variables, var)));
            };


            Supplier<Formula>[] connectives = new Supplier[]{generateEquals, generateAnd, generateOr, generateIf, generateIff, generateNot, generateForAll, generateExists};


            Formula f = connectives[ThreadLocalRandom.current().nextInt(0, connectives.length)].get();
            f.setAssumptions(Sets.with(f));

            return f;
        }


    }


    private Predicate generateRandomPredicate(List<Variable> variables) {

        int arity = ThreadLocalRandom.current().nextBoolean() ? 1 : 2; //For now only upto arity 1 or arity 2 for readability


        String predicateName = arity == 1 ? (Names.UNARY_RELATIONS[ThreadLocalRandom.current().nextInt(0, Names.UNARY_RELATIONS.length)]) :
                               (Names.BINARY_RELATIONS[ThreadLocalRandom.current().nextInt(0, Names.BINARY_RELATIONS.length)]);


        Value[] arguments = new Value[arity];

        for (int i = 0; i < arguments.length; i++) {

            if (variables.size() > 0 && ThreadLocalRandom.current().nextBoolean()) {

                arguments[i] = variables.get(ThreadLocalRandom.current().nextInt(0, variables.size()));

            } else {

                arguments[i] = generateRandomGroundValue(0);
            }

        }

        if (variables.size() > 0) {

            arguments[ThreadLocalRandom.current().nextInt(0, arguments.length)] = variables.get(ThreadLocalRandom.current().nextInt(0, variables.size()));

        }
        return new Predicate(predicateName, arguments);


    }


    private Optional<Formula> randomlyInfer(Set<Formula> formulae, int maxFormulaDepth, int depth) {


        List<Formula> givens = new ArrayList<>(formulae);

        Supplier<Formula> randomFormula = () -> givens.get(ThreadLocalRandom.current().nextInt(givens.size()));

        Function<java.util.function.Predicate<Formula>, Optional<Formula>> randomFormulaSatisfying = (p) -> {
            List<Formula> filtered = givens.stream().filter(p).collect(Collectors.toList());

            if (filtered.isEmpty()) {

                return Optional.empty();

            } else {

                return Optional.of(filtered.get(ThreadLocalRandom.current().nextInt(filtered.size())));
            }
        };

        if (depth > maxInferenceDepth) {

            return Optional.of(randomFormula.get());

        } else {


            Supplier instantiateUniversal = () -> {

                Optional<Formula> someUniversalOpt = randomFormulaSatisfying.apply(f -> f instanceof Universal);

                return someUniversalOpt.map(formula -> {

                    Universal  universal = ((Universal) formula);
                    Variable[] vars      = universal.vars();

                    Formula answer = universal.apply(CollectionUtils.mapWith(vars[0], generateRandomGroundValue(0)));

                    answer.setAssumptions(universal.getAssumptions());

                    return answer;
                });

            };

            Supplier generalizeToUniversal = () -> {

                Formula someCompoundFormula = generateRandomFormula(depth);

                Set<Value> currentValues = formulae.stream().map(Formula::valuesPresent).reduce(Sets.newSet(), Sets::union);
                Set<Value> newValues     = Sets.difference(someCompoundFormula.valuesPresent(), currentValues);

                if (!newValues.isEmpty()) {

                    List<Value> values = new ArrayList<>(newValues);


                    Optional<Formula> conclusionOpt = randomlyInfer(Sets.add(formulae, someCompoundFormula), maxFormulaDepth - 1, depth + 1);


                    if (conclusionOpt.isPresent() && someCompoundFormula.variablesPresent().isEmpty() && conclusionOpt.get().variablesPresent().isEmpty()) {

                        Value value = values.get(ThreadLocalRandom.current().nextInt(values.size()));

                        Variable variable = generateRandomVariable(Sets.union(someCompoundFormula.variablesPresent(), conclusionOpt.get().variablesPresent()));

                        try {
                            Optional.of(new Universal(new Variable[]{variable}, (new Implication(someCompoundFormula, conclusionOpt.get())).replace(value, variable)));

                        } catch (AssertionError e) {

                            int x = 1;
                        }
                        Formula answer = (new Universal(new Variable[]{variable}, (new Implication(someCompoundFormula, conclusionOpt.get())).replace(value, variable)));

                        answer.setAssumptions(Sets.add(conclusionOpt.get().getAssumptions(), someCompoundFormula));
                        return Optional.of(answer);

                    } else {

                        return Optional.empty();
                    }

                } else {

                    return Optional.empty();
                }

            };

            Supplier generateAnd = () -> {

                Formula input1 = randomFormula.get();
                Formula input2 = randomFormula.get();

                Formula conclusion = new And(input1, input2);
                conclusion.setAssumptions(Sets.union(input1.getAssumptions(), input2.getAssumptions()));
                return Optional.of(conclusion);

            };
            Supplier splitAnd = () -> {

                Optional<Formula> someAndOpt = randomFormulaSatisfying.apply(f -> f instanceof And);

                return someAndOpt.map(formula -> {

                    Formula conj = ((And) formula).getArguments()[ThreadLocalRandom.current().nextInt(0, 2)];
                    try {
                        Formula answer = Reader.readFormulaFromString(conj.toString());

                        answer.setAssumptions(formula.getAssumptions());
                        return answer;

                    } catch (Reader.ParsingException e) {
                        e.printStackTrace();
                        throw new AssertionError("");
                    }

                });

            };


            Supplier generateOr1 = () -> {

                Formula input      = randomFormula.get();
                Formula conclusion = new Or(input, generateRandomFormula(maxFormulaDepth));
                conclusion.setAssumptions(input.getAssumptions());
                return Optional.of(conclusion);
            };

            Supplier generateOr2 = () -> {

                Formula input      = randomFormula.get();
                Formula conclusion = new Or(generateRandomFormula(maxFormulaDepth), input);
                conclusion.setAssumptions(input.getAssumptions());
                return Optional.of(conclusion);
            };

            Supplier generateImplication = () -> {
                Formula           randomAssumption = generateRandomFormula(maxFormulaDepth);
                Optional<Formula> conclusionOpt    = randomlyInfer(Sets.add(formulae, randomAssumption), maxFormulaDepth - 1, depth + 1);

                if (conclusionOpt.isPresent() && !formulae.contains(conclusionOpt.get())) {
                    return conclusionOpt.map(formula -> {


                        Formula result = new Implication(randomAssumption, formula);
                        result.setAssumptions(Sets.add(formula.getAssumptions(), randomAssumption));

                        return result;
                    });
                }

                return Optional.empty();

            };


            Supplier<Optional<Formula>>[] inferenceRules = new Supplier[]{splitAnd, splitAnd, splitAnd, generateAnd, generateOr1, generateOr2,
                    instantiateUniversal, instantiateUniversal, instantiateUniversal,
                    instantiateUniversal, generalizeToUniversal, generateImplication, generateImplication};


            Optional<Formula> generated = inferenceRules[ThreadLocalRandom.current().nextInt(inferenceRules.length)].get();

            return generated;


        }


    }


}



