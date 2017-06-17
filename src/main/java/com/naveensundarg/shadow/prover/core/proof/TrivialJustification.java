package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class TrivialJustification extends Justification{


    private Formula formula;
    private Set<Formula> base;
    private String message;
    public TrivialJustification(Set<Formula> base, Formula formula){
        this.formula = formula;
        this.base = base;
        super.name = "Trivial";

        message = "AssumptionsNowContainsGoal";
    }

     public TrivialJustification(Set<Formula> base, Formula formula, String message){
        this.formula = formula;
        this.base = base;
        super.name = "Trivial";

        this.message = message;
    }

    @Override
    public String toString() {
        return "("  + message+ " ("
                + base.stream().map(Formula::toString).reduce("", (x,y) -> x + " "  + y).trim()  +
                ") "
                + formula.toString() + ")";
    }
}
