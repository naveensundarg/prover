package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class TrivialJustification extends Justification{


    private Formula formula;
    private Set<Formula> base;

    public TrivialJustification(Set<Formula> base, Formula formula){
        this.formula = formula;
        this.base = base;
        super.name = "Trivial";
    }

    @Override
    public String toString() {
        return "(AssumptionsNowContainsGoal ["
                + base.stream().map(Formula::toString).reduce("", (x,y) -> x + " "  + y)  +
                "]"
                + formula.toString() + ")";
    }
}
