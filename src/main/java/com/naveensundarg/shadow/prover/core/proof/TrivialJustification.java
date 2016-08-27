package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class TrivialJustification extends Justification{


    private Formula formula;

    public TrivialJustification(Formula formula){
        this.formula = formula;
        super.name = "Trivial";
    }

    @Override
    public String toString() {
        return "TrivialJustification{" +
                "formula=" + formula +
                '}';
    }
}
