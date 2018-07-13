package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Arrays;
import java.util.List;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class AtomicJustification extends Justification {


    private Formula[] inputs;
    public AtomicJustification(String name, Formula... inputs){
        this.name = name;

        this.inputs = inputs;

    }

    public AtomicJustification(String name){
        this.name = name;

        this.inputs = null;

    }

    public Formula[] getInputs() {
        return inputs;
    }

    @Override
    public String toString() {
        if(inputs!=null){

            return "(" + name + Arrays.toString(inputs) + ")";

        } else {

            return name;
        }
    }
}
