package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.representations.Formula;
import com.naveensundarg.shadow.prover.representations.Not;
import com.naveensundarg.shadow.prover.representations.Value;
import com.naveensundarg.shadow.prover.representations.cnf.Clause;
import com.naveensundarg.shadow.prover.representations.cnf.Literal;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class Logic {

    public static Formula negated(Formula f){

        if(f instanceof Not){

            return ((Not) f).getArgument();
        }
        else {
            return new Not(f);
        }
    }



}
