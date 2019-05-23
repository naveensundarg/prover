package com.naveensundarg.shadow.prover.representations.measures;

import com.naveensundarg.shadow.prover.representations.formula.*;

import java.util.Arrays;

public final class RelationSymbols extends Measure {


    public static int measure(Formula formula) {

        if (formula instanceof Atom) {

            return 1;
        }

        if (formula instanceof Predicate) {
            return 1;
        }

        if (formula instanceof And) {

            Formula[] args = ((And) formula).getArguments();

            return Arrays.stream(args).mapToInt(RelationSymbols::measure).sum();
        }

        if (formula instanceof Or) {

            Formula[] args = ((Or) formula).getArguments();

            return Arrays.stream(args).mapToInt(RelationSymbols::measure).sum();

        }

        if (formula instanceof Not) {
            return  measure(((Not) formula).getArgument());
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
}
