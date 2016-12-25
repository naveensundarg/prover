package com.naveensundarg.shadow.prover.core.oscar;

import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Set;

/**
 * Created by naveensundarg on 12/19/16.
 */
public final class Sequent {


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


    public Formula getConclusion() {
        return conclusion;
    }

    public Set<Formula> getSupposition() {
        return supposition;
    }

    public Type getType() {
        return type;
    }

    @Override
    public String toString() {
        return "Sequent{" +
                "conclusion=" + conclusion +
                ", supposition=" + supposition +
                ", type=" + type +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Sequent sequent = (Sequent) o;

        if (!conclusion.equals(sequent.conclusion)) return false;
        if (!supposition.equals(sequent.supposition)) return false;
        return type == sequent.type;
    }

    @Override
    public int hashCode() {
        int result = conclusion.hashCode();
        result = 31 * result + supposition.hashCode();
        result = 31 * result + type.hashCode();
        return result;
    }
}
