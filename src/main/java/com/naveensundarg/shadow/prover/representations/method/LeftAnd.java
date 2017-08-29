package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.And;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class LeftAnd extends PrimitiveMethod {

    private static final LeftAnd INSTANCE;

    static {
        INSTANCE = new LeftAnd();
    }

    private LeftAnd() {

    }

    public static LeftAnd getInstance() {

        return INSTANCE;
    }

    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if(args.size()==1){


            Phrase formula = args.get(0);

            if(formula instanceof Formula){

                if(assumptionBase.contains(formula)){

                    if(formula instanceof And){

                        return ((And) formula).getArguments()[0];

                    } else{

                        return new ErrorPhrase("left-and: requires an conjunction as input");


                    }

                } else{

                    return new ErrorPhrase("left-and: could not find " + formula + " in the assumption base");
                }

            } else {

                return new ErrorPhrase("left-and requires a formula as input, but got " + formula + " of class " + formula.getClass() );
            }




        } else {


            return new ErrorPhrase("left-and requires exactly two arguments");
        }


    }

    @Override
    public String toString() {
        return "left-and";
    }
}
