package com.naveensundarg.shadow.prover.core.proof;

import java.util.List;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class CompoundJustification extends Justification {

    List<Justification> subs;

    public CompoundJustification(String name, List<Justification> subs){
        this.name = name;
        this.subs = subs;

    }

    public List<Justification> getSubs() {
        return subs;
    }

    @Override
    public String toString() {
        return "(" + name +"\n"+
                " "+ subs +")";
    }
}
