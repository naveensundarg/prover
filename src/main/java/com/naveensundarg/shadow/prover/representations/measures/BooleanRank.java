package com.naveensundarg.shadow.prover.representations.measures;

import com.naveensundarg.shadow.prover.representations.formula.*;

import java.util.Arrays;

public final class BooleanRank extends Measure {


    public static int measure(Formula formula) {

        if (formula instanceof Atom) {

            return 0;
        }

        if (formula instanceof Predicate) {
            return 0;
        }

        if (formula instanceof And) {

            Formula[] args = ((And) formula).getArguments();

            return 1 + Arrays.stream(args).mapToInt(BooleanRank::measure).max().orElse(0);
        }

        if (formula instanceof Or) {

            Formula[] args = ((Or) formula).getArguments();

            return 1 + Arrays.stream(args).mapToInt(BooleanRank::measure).max().orElse(0);

        }

        if (formula instanceof Not) {
            return 1 + measure(((Not) formula).getArgument());
        }


        if (formula instanceof Implication) {
            return 1 + Math.max(measure(((Implication) formula).getAntecedent()), measure(((Implication) formula).getConsequent()));
        }

        if (formula instanceof BiConditional) {
            return 1 + Math.max(measure(((BiConditional) formula).getLeft()), measure(((BiConditional) formula).getRight()));
        }


        if (formula instanceof Existential) {
            return measure(((Existential) formula).getArgument());
        }

        if (formula instanceof Universal) {
            return measure(((Universal) formula).getArgument());
        }

        return 0;
    }
}
