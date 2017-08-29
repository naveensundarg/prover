package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public final class Claim extends PrimitiveMethod {

    private static final Claim INSTANCE;

    static {
        INSTANCE = new Claim();
    }

    private Claim() {

    }

    public static Claim getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if (args.size() == 1) {

            Phrase arg = args.get(0);
            if (arg instanceof Formula) {

                if (assumptionBase.contains(arg)) {

                    return arg;

                } else {

                    return new ErrorPhrase("Claim: Assumption base does not contain " + arg);

                }
            } else {


                return new ErrorPhrase("Claim takes only formulae as input but got " + arg + " which is of class " + arg.getClass());

            }


        }
        {

            return new ErrorPhrase("Claim takes exactly one input but got: " + args);
        }

    }

     @Override
    public String toString() {
        return "claim";
    }
}
