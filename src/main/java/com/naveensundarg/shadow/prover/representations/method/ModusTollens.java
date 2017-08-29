package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;
import com.naveensundarg.shadow.prover.representations.formula.Not;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public final class ModusTollens extends PrimitiveMethod {

    private static final ModusTollens INSTANCE;

    static {
        INSTANCE = new ModusTollens();
    }

    private ModusTollens() {

    }

    public static ModusTollens getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if (args.size() == 2) {

            Phrase implication = args.get(0);
            Phrase consequentNegated = args.get(1);

            if (implication instanceof Implication) {

                if (consequentNegated instanceof Formula) {

                    Formula consequentNegatedFormula = (Formula) consequentNegated;

                    if (assumptionBase.contains(implication)) {

                        if (assumptionBase.contains(consequentNegated)) {

                            if (((Implication) implication).getConsequent().equals(Logic.negated(consequentNegatedFormula))) {


                                return new Not(((Implication) implication).getAntecedent());

                            } else {
                                return new ErrorPhrase("modus-tollens: Given formula " + consequentNegated + " does not match the consequent " + ((Implication) implication).getAntecedent());
                            }
                        } else {

                             return new ErrorPhrase("modus-tollens: Antecedent "+ consequentNegated + "  not found in the assumption base");
                        }

                    } else {

                        return new ErrorPhrase("modus-tollens: Implication  "+ implication + " not found in the assumption base");

                    }

                } else {

                    return new ErrorPhrase("modus-tollens takes only formulae as input but got " + consequentNegated + " which is of class " + consequentNegated.getClass());
                }

            } else {

                return new ErrorPhrase("modus-tollens: The first argument should be an implication " + implication + " which is of class " + implication.getClass());

            }
        } else {

            return new ErrorPhrase("modus-tollens takes exactly two inputs but got: " + args);
        }

    }

    @Override
    public String toString() {
        return "modus-tollens";
    }
}
