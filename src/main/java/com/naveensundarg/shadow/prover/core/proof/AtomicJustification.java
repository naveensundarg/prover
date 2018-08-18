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




    @Override
    public String toString() {
        return "(" + name + Arrays.stream(inputs).map(x-> x+"\n").reduce("",(x,y)-> (x + y)) + ")";
    }
}
