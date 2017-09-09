package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import sun.rmi.runtime.Log;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public final class Absurd extends PrimitiveMethod {

    private static final Absurd INSTANCE;

    static {
        INSTANCE = new Absurd();
    }

    private Absurd() {

    }

    public static Absurd getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if (args.size() == 2) {

            Phrase P1 = args.get(0);
            Phrase P2 = args.get(1);

            if (P1 instanceof Formula) {


                if (P2 instanceof Formula) {


                    if (assumptionBase.contains(P1)) {

                        if (assumptionBase.contains(P2)) {

                            if(P1.equals(Logic.negated((Formula) P2))|| P2.equals(Logic.negated((Formula)P1))){


                                return Logic.getFalseFormula();


                            }
                            else {

                                return new ErrorPhrase("absurd: pattern matching failed");
                            }


                        } else {

                            return new ErrorPhrase("absurd: the assumption base does not contain " + P2);
                        }

                    } else {

                        return new ErrorPhrase("absurd: the assumption base does not contain " + P1);
                    }


                } else {

                    return new ErrorPhrase("absurd: the first input is not a formula, got " + P2 + " of class " + P2.getClass());
                }

            } else {

                return new ErrorPhrase("absurd: the first input is not a formula, got " + P1 + " of class " + P1.getClass());
            }

        } else

        {

            return new ErrorPhrase("absurd takes exactly two inputs but got: " + args);
        }

    }

    @Override
    public String toString() {
        return "absurd";
    }
}
