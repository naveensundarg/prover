package com.naveensundarg.shadow.prover.utils;

import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;

/**
 * Created by naveensundarg on 4/13/16.
 */
public class SymbolGenerator {

    private static Map<Problem, Integer> newConstantCount;
    private static Map<Problem, Integer> newVariableCount;
    private static Map<Problem, Integer> newFunctionCount;

    static {
        newConstantCount = newMap();
        newVariableCount = newMap();
        newFunctionCount = newMap();

    }

    public static Value skolem(List<Variable> variables, Problem problem){

        if(variables.isEmpty()){
            return SymbolGenerator.newConstant(problem);
        }
        else{
            return newFunction(variables, problem);
        }
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

    public static synchronized Compound newFunction(List<Variable> variables, Problem problem){
        int count;

        if(newFunctionCount.containsKey(problem)){

            count = newFunctionCount.get(problem);
        } else {

            count = 0;
        }

        newFunctionCount.put(problem, count + 1);

        String name = "Skolem" + count;

        List<Value> valueList = variables.stream().map(x->(Value) x).collect(Collectors.toList());

        return new Compound(name, valueList);

    }


}
