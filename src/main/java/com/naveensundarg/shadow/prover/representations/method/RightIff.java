package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.BiConditional;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class RightIff extends PrimitiveMethod {

    private static final RightIff INSTANCE;

    static {
        INSTANCE = new RightIff();
    }

    private RightIff() {

    }

    public static RightIff getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if(args.size()==1){


            Phrase formula = args.get(0);

            if(formula instanceof Formula){

                if(assumptionBase.contains(formula)){

                    if(formula instanceof BiConditional){

                        BiConditional biConditional = (BiConditional) formula;
                        return new Implication(biConditional.getRight(), biConditional.getLeft());

                    } else{

                        return new ErrorPhrase("right-iff: requires an biconditional as input");


                    }

                } else{

                    return new ErrorPhrase("right-iff: could not find " + formula + " in the assumption base");
                }

            } else {

                return new ErrorPhrase("right-iff requires a formula as input, but got " + formula + " of class " + formula.getClass() );
            }




        } else {


            return new ErrorPhrase("right-iff requires exactly two arguments");
        }


    }

    @Override
    public String toString() {
        return "right-iff";
    }
}
