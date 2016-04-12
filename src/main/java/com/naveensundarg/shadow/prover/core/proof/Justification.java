package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.Formula;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by naveensundarg on 4/8/16.
 */
public abstract class Justification {

    protected String name;


    public String getName() {
        return name;
    }


    public static Justification trivial(Formula formula){
        return new TrivialJustification(formula);
    }

    public static Justification compound(String name, List<Justification> subs){
        return new CompoundJustification(name, subs);
    }

    public static Justification compound(String name, Justification sub){
        List<Justification> subs = new ArrayList<>();
        subs.add(sub);
        return new CompoundJustification(name, subs);
    }



}
