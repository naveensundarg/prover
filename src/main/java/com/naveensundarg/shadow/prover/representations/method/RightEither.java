package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Or;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class RightEither extends PrimitiveMethod {

    private static final RightEither INSTANCE;

    static {
        INSTANCE = new RightEither();
    }

    private RightEither() {

    }

    public static RightEither getInstance() {

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

                    if(assumptionBase.contains(f2) ){

                        return new Or(f1, f2);
                    } else {

                        return new ErrorPhrase("right-either: could not find "+ P2 + " in the assumption base ");
                    }


                } else{

                    return new ErrorPhrase("right-either: the second argument is not a formula. Got " + P2  + " of class " + P2.getClass());

                }

            } else{

                return new ErrorPhrase("right-either: the first argument is not a formula. Got " + P1  + " of class " + P1.getClass());

            }


        } else {


            return new ErrorPhrase("right-either requires at exactly two arguments, but got: " +  args);
        }


    }

    @Override
    public String toString() {
        return "right-either";
    }
}
