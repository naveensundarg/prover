package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class FalseElim extends PrimitiveMethod {

    private static final FalseElim INSTANCE;

    static {
        INSTANCE = new FalseElim();
    }

    private FalseElim() {

    }

    public static FalseElim getInstance() {

        return INSTANCE;
    }
    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {
        if(args.size()==0){

            return Logic.negated(Logic.getFalseFormula());


        } else {

            return new ErrorPhrase("FalseElim takes zero arguments but got: "+ args);
        }
    }


    @Override
    public String toString() {
        return "false-elim";
    }
}
