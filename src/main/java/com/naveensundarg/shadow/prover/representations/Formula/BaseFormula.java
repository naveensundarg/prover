package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Value;

import java.util.Set;

/**
 * Created by naveensundarg on 12/5/16.
 */
public interface BaseFormula {

    Set<Value> allValues();
    String getName();
}
