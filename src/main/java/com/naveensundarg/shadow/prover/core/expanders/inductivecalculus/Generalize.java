package com.naveensundarg.shadow.prover.core.expanders.inductivecalculus;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.core.Prover;
import com.naveensundarg.shadow.prover.core.internals.Expander;
import com.naveensundarg.shadow.prover.core.proof.Unifier;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Constants;
import com.naveensundarg.shadow.prover.utils.Reader;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public enum Generalize implements Expander {

    INSTANCE;

    @Override
    public void expand(Prover prover, Set<Formula> base, Set<Formula> added, Formula goal) {


        Set<Exemplar> exemplars = base.
                stream().
                filter(f -> f instanceof Exemplar).
                map(f -> (Exemplar) f).
                collect(Collectors.toSet());

        Set<Formula> derived = exemplars.stream().map(this::generalize).collect(Collectors.toSet());
        if (!base.containsAll(derived)) {
         //   prover.getLogger().expansionLog("Exemplar", Sets.newSet());

        }

        base.addAll(derived);
        added.addAll(derived);

    }

    static Formula BLANK = Logic.getTrueFormula();

    static {
        try {
            BLANK = Reader.readFormulaFromString("_");
        } catch (Reader.ParsingException e) {
            e.printStackTrace();
        }
    }

    private Formula generalize(Exemplar exemplar) {


        Formula input = exemplar.getInput();
        Formula output = exemplar.getOutput();

        Set<Value> inputValues = input.valuesPresent();
        Set<Value> outputValues = output.valuesPresent();

        Set<Formula> inputFormulae = input.subFormulae();
        Set<Formula> outputFormulae = output.subFormulae();

        Set<Value> commonValues = Sets.union(inputValues, outputValues);
        Set<Formula> commonFormula = Logic.getNonSubFormulae(Sets.intersection(inputFormulae, outputFormulae));

        Set<Value> nonGeneralizableValues = commonFormula.stream().map(Formula::valuesPresent).reduce(Sets.newSet(), Sets::union);
        Set<Value> generalizableValues = Sets.difference(commonValues, nonGeneralizableValues);

        AtomicInteger variableId  = new AtomicInteger(1);
        Map<Value, Variable> valueGeneralizationMap = generalizableValues.stream().collect(Collectors.toMap(
                e -> e, e -> new Variable("$x" + variableId.getAndIncrement() +"?" )
        ));

        variableId.set(1);
        Map<Formula, Variable> formulaGeneralizationMap = commonFormula.stream().collect(Collectors.toMap(
                e -> e, e -> new Variable("$f" + variableId.getAndIncrement() +"?" )
        ));

        variableId.set(1);

        variableId.set(1);
        Map<Formula, Formula> formulaReplacementMap = commonFormula.stream().collect(Collectors.toMap(
                e -> e, e -> new Predicate("$f" + variableId.getAndIncrement() +"?" )
        ));
        new HashSet<>(valueGeneralizationMap.values());
        Variable[] variables = new Variable[]{};


        variables = Sets.union(new HashSet<>(valueGeneralizationMap.values()),
                new HashSet<>(formulaGeneralizationMap.values())).toArray(variables);

        Formula valueGeneralizedInput = input.generalize(valueGeneralizationMap);
        Formula valueGeneralizedOutput = output.generalize(valueGeneralizationMap);

        for(Map.Entry<Formula, Formula> entry : formulaReplacementMap.entrySet()){

            valueGeneralizedInput = valueGeneralizedInput.replaceSubFormula(entry.getKey(), entry.getValue());
        }

        for(Map.Entry<Formula, Formula> entry : formulaReplacementMap.entrySet()){

            valueGeneralizedOutput = valueGeneralizedOutput.replaceSubFormula(entry.getKey(), entry.getValue());
        }


        Formula inferred = new Universal(
                variables,
                new Implication(valueGeneralizedInput, valueGeneralizedOutput));

        return inferred;
    }

}
