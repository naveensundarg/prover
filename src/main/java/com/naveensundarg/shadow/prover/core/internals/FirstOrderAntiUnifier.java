package com.naveensundarg.shadow.prover.core.internals;

import clojure.lang.Var;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.CommonUtils;
import com.naveensundarg.shadow.prover.utils.Holder;
import com.naveensundarg.shadow.prover.utils.Reader;
import org.apache.commons.lang3.tuple.Pair;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.mapWith;


public class FirstOrderAntiUnifier {


    public static Value  antiUnify(List<Value> inputs) {


        return  antiUnify(inputs, new AtomicInteger(0));

    }

    public static Value  antiUnify(List<Value> inputs, AtomicInteger variableNumber ) {

        Map<Value, Variable> stack = CollectionUtils.newMap();

        return inputs.stream().reduce(inputs.get(0), (left, right)-> {

            Pair<Value, Map<Value, Variable>> genResults = antiUnify(left, right, variableNumber, stack);

            stack.putAll(genResults.getRight());

            return genResults.getLeft();
        });

    }

    public static Pair<Value, Map<Value, Variable>>  antiUnify(Value left, Value right) {

        return antiUnify(left, right, new AtomicInteger(1), CollectionUtils.newMap());

    }

    public static Pair<Value, Map<Value, Variable>> antiUnify(Value left, Value right, AtomicInteger variableNumber, Map<Value, Variable> stack) {

        if (left.equals(right)) {

            return Pair.of(left, CollectionUtils.newMap());
        }

        //TODO: This may not work always!
        if (left instanceof Variable && right instanceof Variable) {

            if (left.getName().compareTo(right.getName()) > 0) {

                return Pair.of(left, mapWith(right,(Variable) left));

            } else {

                return Pair.of(right, mapWith(left,(Variable) right));

            }
        }

        if (left instanceof Variable) {

            Map<Variable, Set<Value>> bindings = CommonUtils.reverseMapSet(stack);

            if(bindings.containsKey(left) && !bindings.get(left).contains(right)){


                Variable var =  (Variable) CommonUtils.readValueFromString("?x" + variableNumber.getAndIncrement());

                return Pair.of(var, mapWith(left, var, right, var));

            }

            return Pair.of(left, mapWith(right, (Variable) left));
        }
        if (right instanceof Variable) {

            Map<Variable, Set<Value>> bindings = CommonUtils.reverseMapSet(stack);

            if(bindings.containsKey(right) && !bindings.get(right).contains(right)){


                Variable var =  (Variable) CommonUtils.readValueFromString("?x" + variableNumber.getAndIncrement());

                return Pair.of(var, mapWith(left, var, right, var));

            }
            return Pair.of(right, mapWith(right, (Variable) right));
        }

        if(left instanceof Constant && right instanceof Constant){


            Variable var =  (Variable) CommonUtils.readValueFromString("?x" + variableNumber.getAndIncrement());

            return Pair.of(var, mapWith(left, var, right, var));
        }


        if (left instanceof Compound && right instanceof Compound) {

            if ((!left.getName().equals(right.getName())) || (left.getArguments().length != right.getArguments().length)) {

                Variable var =  (Variable) CommonUtils.readValueFromString("?x" + variableNumber.getAndIncrement());
                return Pair.of(var, mapWith(left, var, right, var));

            }



            Value[] generalValues = new Value[left.getArguments().length];

            for (int i = 0; i < left.getArguments().length; i++) {

                Value leftValue = left.getArguments()[i];
                Value rightValue = right.getArguments()[i];

                Pair<Value, Map<Value, Variable>> generalizedArg = antiUnify(leftValue.generalize(stack), rightValue.generalize(stack), variableNumber, stack);


                generalValues[i] = generalizedArg.getLeft();
                stack.putAll(generalizedArg.getRight());
            }

            return Pair.of(new Compound(left.getName(), generalValues), stack);


        }

        Variable var =  (Variable) CommonUtils.readValueFromString("?x" + variableNumber.getAndIncrement());


        return Pair.of(var, mapWith(left, var, right, var));


    }
}
