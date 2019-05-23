package com.naveensundarg.shadow.prover.representations.measures;

import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Time;
import com.naveensundarg.shadow.prover.representations.value.Value;

import java.util.Arrays;

public final class FunctionSymbols extends Measure {


    public static int measure(Formula formula) {

        if (formula instanceof Atom) {

            return 0;
        }

        if (formula instanceof Predicate) {
            Value[] arguments = ((Predicate) formula).getArguments();
            return Arrays.stream(arguments).mapToInt(FunctionSymbols::measure).sum();
        }

        if (formula instanceof And) {

            Formula[] args = ((And) formula).getArguments();

            return Arrays.stream(args).mapToInt(FunctionSymbols::measure).sum();
        }

        if (formula instanceof Or) {

            Formula[] args = ((Or) formula).getArguments();

            return Arrays.stream(args).mapToInt(FunctionSymbols::measure).sum();

        }

        if (formula instanceof Not) {
            return measure(((Not) formula).getArgument());
        }


        if (formula instanceof Implication) {
            return measure(((Implication) formula).getAntecedent()) +
                    measure(((Implication) formula).getConsequent());
        }

        if (formula instanceof BiConditional) {
            return measure(((BiConditional) formula).getLeft()) +
                    measure(((BiConditional) formula).getRight());
        }


        if (formula instanceof Existential) {
            return measure(((Existential) formula).getArgument());
        }

        if (formula instanceof Universal) {
            return measure(((Universal) formula).getArgument());
        }

        return 0;
    }

    public static int measure(Value value) {

        if (value instanceof Constant) {
            return 1;

        }

        if (value instanceof Compound) {

            Value[] arguments = value.getArguments();

            return Arrays.stream(arguments).mapToInt(FunctionSymbols::measure).sum();
        }
        return 0;

    }
}