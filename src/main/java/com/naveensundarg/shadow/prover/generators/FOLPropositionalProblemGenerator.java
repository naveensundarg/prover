package com.naveensundarg.shadow.prover.generators;


import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.SnarkWrapper;
import com.naveensundarg.shadow.prover.core.proof.AtomicJustification;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.ImmutablePair;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import javax.swing.text.html.Option;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class FOLPropositionalProblemGenerator {

    static Prover prover;

    private final int maxLiteralsInClause;
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
        generatorParams.maxTermDepth = 1;
        generatorParams.maxFormulaDepth = 2;
        generatorParams.maxInferenceDepth = 8;

        FOLPropositionalProblemGenerator folPropositionalProblemGenerator = new FOLPropositionalProblemGenerator(generatorParams);

        Pair<Set<Formula>, Formula> problem = folPropositionalProblemGenerator.generateProblem();


        problem.first().forEach(System.out::println);

        System.out.println("------");

        System.out.println(problem.second());
    }

    private FOLPropositionalProblemGenerator(GeneratorParams generatorParams) {

        this.maxAtoms = generatorParams.maxAtoms;
        this.maxLiteralsInClause = generatorParams.maxLiteralsInClause;
        this.totalClauses = generatorParams.clauses;
        this.maxArity = generatorParams.maxArity;
        this.maxTermDepth = generatorParams.maxTermDepth;
        this.maxFormulaDepth = generatorParams.maxFormulaDepth;
        this.maxInferenceDepth = generatorParams.maxInferenceDepth;
    }


    static {

    }


    private Pair<Set<Formula>, Formula> generateProblem() {

        List<Formula> clauses = CollectionUtils.newEmptyList();

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


        return ImmutablePair.from(formulae, f);

    }


    private Constant generateRandomConstant() {

        return new Constant(Names.CONSTANTS[ThreadLocalRandom.current().nextInt(0, Names.CONSTANTS.length)]);

    }

    private Variable generateRandomVariable() {

        return new Variable("?x" + ThreadLocalRandom.current().nextInt(0,100));

    }

    private Variable generateRandomVariable(Set<Variable> exclusions) {

        List<String> scope = new ArrayList<>(Sets.difference(Arrays.stream(Names.VARIABLES).collect(Collectors.toSet()), exclusions.stream().map(Variable::toString).collect(Collectors.toSet())));


        return new Variable("?x" + ThreadLocalRandom.current().nextInt(0,100));

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

            return generateRandomPredicate(variables);
        } else {


            Supplier generateAnd = () -> new And(generateRandomFormula(depth + 1, variables), generateRandomFormula(depth + 1, variables));
            Supplier generateOr = () -> new Or(generateRandomFormula(depth + 1, variables), generateRandomFormula(depth + 1, variables));
            Supplier generateIf = () -> new Implication(generateRandomFormula(depth + 1, variables), generateRandomFormula(depth + 1, variables));
            Supplier generateIff = () -> new BiConditional(generateRandomFormula(depth + 1, variables), generateRandomFormula(depth + 1, variables));
            Supplier generateNot = () -> new Not(generateRandomFormula(depth + 1, variables));
            Supplier generateEquals = () -> new Predicate("=", new Value[]{generateRandomGroundValue(0), generateRandomGroundValue(0)});


            Supplier generateForAll = () -> {

                Variable var = generateRandomVariable();
                return new Universal(new Variable[]{var}, generateRandomFormula(depth + 1, CollectionUtils.addToList(variables, var)));
            };

            Supplier generateExists = () -> {

                Variable var = generateRandomVariable();
                return new Existential(new Variable[]{var}, generateRandomFormula(depth + 1, CollectionUtils.addToList(variables, var)));
            };


            Supplier<Formula>[] connectives = new Supplier[]{generateEquals, generateAnd, generateOr, generateIf, generateIff, generateNot, generateForAll, generateExists};


            return connectives[ThreadLocalRandom.current().nextInt(0, connectives.length)].get();
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


        List<Formula> givens = formulae.stream().collect(Collectors.toList());

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

            Supplier generateAnd = () -> Optional.of(new And(randomFormula.get(), randomFormula.get()));
            Supplier splitAnd = () -> {

                Optional<Formula> someAnd = randomFormulaSatisfying.apply(f -> f instanceof And);


                if (someAnd.isPresent()) {

                    Formula[] args = ((And) someAnd.get()).getArguments();

                    return Optional.of(args[ThreadLocalRandom.current().nextInt(args.length)]);
                } else {

                    return Optional.empty();
                }

            };

            Supplier instantiateUniversal = () -> {

                Optional<Formula> someUniversal = randomFormulaSatisfying.apply(f -> f instanceof Universal);


                if (someUniversal.isPresent()) {

                    Universal universal = ((Universal) someUniversal.get());
                    Variable[] vars = universal.vars();

                    return Optional.of(someUniversal.get().apply(CollectionUtils.mapWith(vars[0], generateRandomGroundValue(0))));
                } else {

                    return Optional.empty();
                }

            };

            Supplier generalizeToUniversal = () -> {

                Optional<Formula> someCompoundFormula = randomFormulaSatisfying.apply(f -> !f.valuesPresent().isEmpty());


                if (someCompoundFormula.isPresent()) {

                    List<Value> values = new ArrayList<>(someCompoundFormula.get().valuesPresent());

                    Value value = values.get(ThreadLocalRandom.current().nextInt(values.size()));
                    Variable variable = generateRandomVariable(someCompoundFormula.get().variablesPresent());
                    return Optional.of(new Universal(new Variable[]{variable}, someCompoundFormula.get().replace(value, variable)));
                } else {

                    return Optional.empty();
                }

            };

            Supplier generateOr1 = () -> Optional.of(new Or(randomFormula.get(), generateRandomFormula(maxFormulaDepth)));
            Supplier generateOr2 = () -> Optional.of(new Or(generateRandomFormula(maxFormulaDepth), randomFormula.get()));

            Supplier generateAssumption = () -> {
                Formula randomAssumption = generateRandomFormula(maxFormulaDepth);
                Optional<Formula> conclusionOpt = randomlyInfer(Sets.add(formulae, randomAssumption), maxFormulaDepth - 1, depth + 1);

                return conclusionOpt.map(formula -> new Implication(randomAssumption, formula));
            };


            Supplier<Optional<Formula>>[] inferenceRules = new Supplier[]{splitAnd, generateAnd, generateOr1, generateOr2, instantiateUniversal, generateAssumption};


            List<Formula> generated = Arrays.stream(inferenceRules).map(Supplier::get).filter(Optional::isPresent).map(Optional::get).collect(Collectors.toList());

            if (generated.isEmpty()) {

                return Optional.empty();
            } else {

                return Optional.of(generated.get(ThreadLocalRandom.current().nextInt(generated.size())));

            }


        }


    }


}



