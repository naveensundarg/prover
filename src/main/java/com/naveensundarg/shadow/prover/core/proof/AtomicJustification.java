package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class AtomicJustification extends Justification {


    private Set<Formula> inputs;
    public AtomicJustification(String name, Formula... inputs){
        this.name = name;

        this.inputs = Arrays.stream(inputs).collect(Collectors.toSet());

    }

    public AtomicJustification(String name, Set<Formula> inputs){
        this.name = name;
        this.inputs = inputs;

    }



    public Formula[] getInputs() {
        return inputs.toArray(new Formula[0]);
    }

    @Override
    public String toString() {
        if(inputs!=null){

            return "(" + name + (inputs) + ")";

        } else {

            return name;
        }
    }
}
