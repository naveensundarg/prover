package com.naveensundarg.shadow.prover.representations.formula;

import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.Map;

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
