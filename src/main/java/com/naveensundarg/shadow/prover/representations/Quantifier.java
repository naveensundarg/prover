package com.naveensundarg.shadow.prover.representations;

import com.naveensundarg.shadow.prover.core.SymbolGenerator;
import com.naveensundarg.shadow.prover.utils.ImmutablePair;
import com.naveensundarg.shadow.prover.utils.Pair;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;

/**
 * Created by naveensundarg on 4/13/16.
 */
public interface Quantifier {

    Variable[] vars();

    Formula getArgument();

    default Formula renamed(Map<Variable, Variable> variableMap){

        Variable[] vars = vars();


        Variable[] newVars = new Variable[vars.length];

        for(int i = 0; i<vars.length; i++){

            newVars[i] = variableMap.get(vars[i]);
        }


        if(this instanceof Existential){

            return  new Existential(newVars, getArgument());
        }
        else{

            return  new Universal(newVars, getArgument());
        }

    }

}
