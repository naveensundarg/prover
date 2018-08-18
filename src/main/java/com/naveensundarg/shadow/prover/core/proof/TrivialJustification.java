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

    private String printFormula(Formula f){

        if(f.getJustification()==null){

            return f.toString();
        }
        else {

            return "\n(" + f.toString()  + "\n    "   + f.getJustification() + ")";
        }
    }
    @Override
    public String toString() {
        return "("  + message+ "\n Givens:\n("
                + base.stream().map(this::printFormula).reduce("", (x, y) -> x + "\n"  + y).trim()  +
                ") \n Goals:\n"
                + formula.toString() + ")";
    }

    public Formula getFormula() {
        return formula;
    }

    public Set<Formula> getBase() {
        return base;
    }

    public String getMessage() {
        return message;
    }
}
