package com.naveensundarg.shadow.prover.representations.deduction;

import com.naveensundarg.shadow.prover.representations.Phrase;

/**
 * Created by naveensundarg on 8/27/17.
 */
public final class Assume extends Deduction {

    private final Phrase assumption;
    private final Deduction deduction;
    public Assume(Phrase assumption, Deduction deduction){

        this.assumption = assumption;
        this.deduction = deduction;
    }

    public Phrase getAssumption() {
        return assumption;
    }

    public Deduction getDeduction() {
        return deduction;
    }

    @Override
    public String toString() {
        return "(assume "  + assumption +  " " + deduction + ")";
    }
}
