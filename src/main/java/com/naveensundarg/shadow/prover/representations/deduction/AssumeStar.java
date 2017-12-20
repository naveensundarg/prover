package com.naveensundarg.shadow.prover.representations.deduction;

import com.naveensundarg.shadow.prover.representations.Phrase;

import java.util.List;

/**
 * Created by naveensundarg on 8/27/17.
 */
public final class AssumeStar extends Deduction {

    private final List<Phrase> assumptions;
    private final Deduction deduction;
    public AssumeStar(List<Phrase> assumptions, Deduction deduction){

        this.assumptions = assumptions;
        this.deduction = deduction;
    }

    public List<Phrase> getAssumption() {
        return assumptions;
    }

    public Deduction getDeduction() {
        return deduction;
    }

    @Override
    public String toString() {
        return "(assume* "  + assumptions.stream().map(Object::toString).reduce(" ",(x,y)-> x + " "+ y) +  " " + deduction + ")";
    }
}
