package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Not;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class DoubleNegation extends PrimitiveMethod{

    private static final DoubleNegation INSTANCE;

    static {
        INSTANCE = new DoubleNegation();
    }

    private DoubleNegation() {

    }

    public static DoubleNegation getInstance() {

        return INSTANCE;
    }
    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if(args.size()==1){

            Phrase arg = args.get(0);

            if(arg instanceof Formula){

                Formula formula = (Formula) arg;

                if(formula instanceof Not && ((Not) formula).getArgument() instanceof Not){

                    return ((Not) ((Not) formula).getArgument()).getArgument();


                } else {
                    return new ErrorPhrase("double negation expects a doubly negated formula");
                }

            } else{

                return new ErrorPhrase("DoubleNegation expects a formula but got: "+ arg);

            }


        } else {

            return new ErrorPhrase("DoubleNegation expects only one argument but got: "+ args);
        }
    }

     @Override
    public String toString() {
        return "double-negation";
    }
}
