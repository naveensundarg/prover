package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.Formula;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Created by naveensundarg on 4/8/16.
 */
public abstract class Justification {

    protected String name;


    public String getName() {
        return name;
    }


    public static Justification trivial(Set<Formula> base, Formula formula){
        return new TrivialJustification(base, formula);
    }

    public static Justification compound(String name, List<Justification> subs){
        return new CompoundJustification(name, subs);
    }

    public static Justification atomic(String name, Formula... inputs){

        return new AtomicJustification(name, inputs);
    }

    public static Justification compound(String name, Justification sub){
        List<Justification> subs = new ArrayList<>();
        subs.add(sub);
        return new CompoundJustification(name, subs);
    }



}
