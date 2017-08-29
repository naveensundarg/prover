package com.naveensundarg.shadow.prover.representations.method;

import com.naveensundarg.shadow.prover.core.Logic;
import com.naveensundarg.shadow.prover.representations.ErrorPhrase;
import com.naveensundarg.shadow.prover.representations.Phrase;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Reader;

import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 8/28/17.
 */
public class TrueIntro extends PrimitiveMethod {

    private static final TrueIntro INSTANCE;

    static {
        INSTANCE = new TrueIntro();
    }

    private TrueIntro() {

    }

    public static TrueIntro getInstance() {

        return INSTANCE;
    }
    @Override
    public Phrase apply(Set<Formula> assumptionBase, List<Phrase> args) {

        if(args.size()==0){

            return Logic.getTrueFormula();


        } else {

            return new ErrorPhrase("true-intro takes no arguments but got: "+ args);
        }
    }

    @Override
    public String toString() {
        return "true-intro";
    }
}
