package com.naveensundarg.shadow.prover.core.oscar;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

/**
 * Created by naveensundarg on 12/19/16.
 */
public class Sequent {


    private enum Type {CONCLUSION, INTEREST, ULTIMATE_INTEREST};


    private final Formula conclusion;
    private final Set<Formula> supposition;
    private final Type type;

    private Sequent(Set<Formula> premises, Formula conclusion, Type type) {

        this.supposition = premises;
        this.conclusion = conclusion;
        this.type = type;
    }


    public static Sequent conclusionSequent(Formula formula){

        return new Sequent(Sets.newSet(), formula, Type.CONCLUSION);
    }

    public static Sequent conclusionSequent(Set<Formula> supposition, Formula formula){

        return new Sequent(supposition, formula, Type.CONCLUSION);
    }




}
