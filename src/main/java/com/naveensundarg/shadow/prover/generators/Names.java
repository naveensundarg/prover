package com.naveensundarg.shadow.prover.generators;

import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Map;

public class Names {


    public static final String[] NAMES= {"a", "b", "c", "d", "e"};

    public static final Map<String,Integer> NAME_TO_INT = CollectionUtils.newMap();


    static {

        NAME_TO_INT.put("a", 1);
        NAME_TO_INT.put("b", 1);
        NAME_TO_INT.put("c", 1);
        NAME_TO_INT.put("d", 1);
        NAME_TO_INT.put("e", 1);

    }
}
