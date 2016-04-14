package com.naveensundarg.shadow.prover.core;

import com.naveensundarg.shadow.prover.representations.Constant;
import com.naveensundarg.shadow.prover.representations.Variable;

import java.util.Map;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;

/**
 * Created by naveensundarg on 4/13/16.
 */
public class SymbolGenerator {

    private static Map<Problem, Integer> newConstantCount;
    private static Map<Problem, Integer> newVariableCount;

    static {
        newConstantCount = newMap();
        newVariableCount = newMap();

    }


    public static synchronized Constant newConstant(Problem problem){
        int count;

        if(newConstantCount.containsKey(problem)){

            count = newConstantCount.get(problem);
        } else {

            count = 0;
        }

        newConstantCount.put(problem, count + 1);

        return new Constant("c" + count);

    }

    public static synchronized Variable newVariable(Problem problem){
        int count;

        if(newVariableCount.containsKey(problem)){

            count = newVariableCount.get(problem);
        } else {

            count = 0;
        }

        newVariableCount.put(problem, count + 1);

        return new Variable("?x" + count);

    }

}
