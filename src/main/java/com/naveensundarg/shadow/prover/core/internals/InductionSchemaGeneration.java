package com.naveensundarg.shadow.prover.core.internals;

import com.naveensundarg.shadow.prover.representations.formula.And;
import com.naveensundarg.shadow.prover.representations.formula.Formula;
import com.naveensundarg.shadow.prover.representations.formula.Implication;
import com.naveensundarg.shadow.prover.representations.formula.Universal;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class InductionSchemaGeneration {


    private static Formula schema(Formula formula, Value current, Value min, Value next){

        Variable x = new Variable("?x");

        Variable y = new Variable("?y");

        return new And(formula.replace(current, min),
                        new Implication(new Universal(new Variable[]{y}, new Implication(formula.replace(current, x), formula.replace(current, next.replace(min, x)))),
                        new Universal(new Variable[]{x}, formula.replace(current, x))));


    }

    public static Set<Formula> generate(Formula formula){

        Set<Value> values =  formula.valuesPresent();
        List<Value> vals = values.stream().sorted(Comparator.comparingInt(Value::getWeight)).collect(Collectors.toList());



        if(vals.size()>= 2){

            Value min = vals.get(0);
            Value next = vals.get(1);

            return  values.stream().map(value -> {

                return schema(formula, value, min, next);

            }).collect(Collectors.toSet());

        } else {

            return Sets.newSet();
        }


    }
}
