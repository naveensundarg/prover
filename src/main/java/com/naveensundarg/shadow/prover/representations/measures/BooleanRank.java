package com.naveensundarg.shadow.prover.representations.measures;

import com.naveensundarg.shadow.prover.representations.formula.*;

import java.util.Arrays;

public final class QuantifierRank extends Measure {


    public static int measure(Formula formula) {

        if (formula instanceof Atom) {

            return 0;
        }

        if (formula instanceof Predicate) {
            return 0;
        }

        if (formula instanceof And) {

            Formula[] args = ((And) formula).getArguments();

            return Arrays.stream(args).mapToInt(QuantifierRank::measure).max().orElse(0);
        }

        if (formula instanceof Or) {

            Formula[] args = ((Or) formula).getArguments();

            return Arrays.stream(args).mapToInt(QuantifierRank::measure).max().orElse(0);

        }

        if (formula instanceof Not) {
            return measure(((Not) formula).getArgument());
        }


        if (formula instanceof Implication) {
            return Math.max(measure(((Implication) formula).getAntecedent()), measure(((Implication) formula).getConsequent()));
        }

        if (formula instanceof BiConditional) {
            return Math.max(measure(((BiConditional) formula).getLeft()), measure(((BiConditional) formula).getRight()));
        }


        if (formula instanceof Existential) {
            return 1 + measure(((Existential) formula).getArgument());
        }

        if (formula instanceof Universal) {
            return 1 + measure(((Universal) formula).getArgument());
        }

        return 0;
    }
}
