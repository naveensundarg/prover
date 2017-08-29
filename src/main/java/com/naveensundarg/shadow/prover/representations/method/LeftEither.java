package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.And;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Or;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class LeftEither extends PrimitiveMethod {

    private static final LeftEither INSTANCE;

    static {
        INSTANCE = new LeftEither();
    }

    private LeftEither() {

    }

    public static LeftEither getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if(args.size()==2){

            Phrase P1 = args.get(0);
            Phrase P2 = args.get(1);

            if (P1 instanceof Formula) {

                if(P2 instanceof Formula){

                    Formula f1 = (Formula) P1;
                    Formula f2 = (Formula) P2;

                    if(assumptionBase.contains(f1) ){

                        return new Or(f1, f2);
                    } else {

                        return new ErrorPhrase("left-either: could not find "+ P1 + " in the assumption base ");
                    }


                } else{

                    return new ErrorPhrase("left-either: the second argument is not a formula. Got " + P2  + " of class " + P2.getClass());

                }

            } else{

                return new ErrorPhrase("left-either: the first argument is not a formula. Got " + P1  + " of class " + P1.getClass());

            }


        } else {


            return new ErrorPhrase("left-either requires at exactly two arguments, but got: " +  args);
        }


    }

    @Override
    public String toString() {
        return "left-either";
    }
}
