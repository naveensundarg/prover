package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Set;

public class Context {


    private final String label;
    private final Set<Formula> formulae;

    public Context(String label, Set<Formula> formulae){

        this.label = label;
        this.formulae = formulae;
    }

    public String getLabel() {
        return label;
    }

    public Set<Formula> getFormulae() {
        return formulae;
    }
}
