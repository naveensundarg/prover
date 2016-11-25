package com.naveensundarg.shadow.prover.core.sortsystem;

import java.util.Arrays;

/**
 * Created by naveensundarg on 11/23/16.
 */
 class Functor {

    private final String name;
    private final Category[] type;

    public Functor(String name, Category[] type) {

        this.name = name;
        this.type = Arrays.copyOf(type, type.length);

    }
}
