package com.naveensundarg.shadow.prover.generators;

import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Map;

public class Names {

    //TODO: Read these from a config file.

    public static final String[] NAMES= {"a", "b", "c", "d", "e"};

    public static final String[] CONSTANTS= {"lever", "axel", "gear", "wheel", "button", "light", "dial", "mirror"};

    public static final String[] UNARY_RELATIONS= {"Broken", "Fixed", "Operational", "New"};
    public static final String[] TYPES= {"Gear", "Box"};

    public static final String[] BINARY_RELATIONS= {"ConnectedTo", "Overrides", "MakesObselete", "Fixes"};

    public static final String[] UNARY_FUNCTIONS = {"enclosingPart"};


    public static final String[] VARIABLES = {"?p1", "?q1", "?r1", "?s1", "?t1", "?x1", "?y1", "?z1",
            "?p2", "?q2", "?r2", "?s2", "?t2", "?x2", "?y2", "?z2"};

    private static final Map<String,Integer> NAME_TO_INT = CollectionUtils.newMap();


    static {

        NAME_TO_INT.put("a", 1);
        NAME_TO_INT.put("b", 1);
        NAME_TO_INT.put("c", 1);
        NAME_TO_INT.put("d", 1);
        NAME_TO_INT.put("e", 1);

    }
}
