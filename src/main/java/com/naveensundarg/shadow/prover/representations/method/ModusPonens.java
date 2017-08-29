package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public final class ModusPonens extends PrimitiveMethod {

    private static final ModusPonens INSTANCE;

    static {
        INSTANCE = new ModusPonens();
    }

    private ModusPonens() {

    }

    public static ModusPonens getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if (args.size() == 2) {

            Phrase implication = args.get(0);
            Phrase antecedent = args.get(1);

            if (implication instanceof Implication) {

                if (antecedent instanceof Formula) {

                    if (assumptionBase.contains(implication)) {

                        if (assumptionBase.contains(antecedent)) {

                            if (((Implication) implication).getAntecedent().equals(antecedent)) {


                                return ((Implication) implication).getConsequent();

                            } else {

                                return new ErrorPhrase("modus-ponens: Given formula " + antecedent + " does not match the antecedent " + ((Implication) implication).getAntecedent());
                            }
                        } else {

                             return new ErrorPhrase("modus-ponens: Antecedent "+ antecedent + "  not found in the assumption base");
                        }

                    } else {

                        return new ErrorPhrase("modus-ponens: Implication  "+ implication + "not found in the assumption base");

                    }

                } else {

                    return new ErrorPhrase("modus-ponens takes only formulae as input but got " + antecedent + " which is of class " + antecedent.getClass());
                }

            } else {

                return new ErrorPhrase("modus-ponens: The first argument should be an implication, but got " + implication + " which is of class " + implication.getClass());

            }
        } else {

            return new ErrorPhrase("modus-ponens takes exactly two inputs but got: " + args);
        }

    }

     @Override
    public String toString() {
        return "modus-ponens";
    }
}
