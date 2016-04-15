package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.Predicate;
import com.naveensundarg.shadow.prover.representations.Value;
import com.naveensundarg.shadow.prover.representations.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;

import java.util.Arrays;
import java.util.Map;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Unifier {


    public static Map<Variable, Value> unify(Predicate p1, Predicate p2) {


        if(!p1.getName().equals(p2.getName())){
            return null;
        }

        if(p1.getArguments().length!= p2.getArguments().length){
            return null;
        }


        Value[] args1 = p1.getArguments();
        Value[] args2 = p2.getArguments();

        return unify(args1, args2, newMap(), 0);

    }



    public static Map<Variable, Value> unify(Value v1, Value v2) {


        return unify(v1, v2, newMap());

    }


    private static Map<Variable, Value> unify(Value x, Value y, Map<Variable, Value> theta) {

        if (theta == null) {
            return null;
        }

        if (x.equals(y)) {
            return theta;
        } else if (x.isVariable()) {
            Variable xVar = (Variable) x;
            return unifyVar(xVar, y, theta);
        } else if (y.isVariable()) {

            Variable yVar = (Variable) y;
            return unifyVar(yVar, x, theta);

        }
        else if (x.isCompound() && y.isCompound()){

            if(x.getName().equals(y.getName())){

                return unify(x.getArguments(), y.getArguments(), theta);

            }

        }


        return null;

    }

    private static Map<Variable, Value> unify(Value[] arr1, Value[] arr2, Map<Variable, Value> theta) {

        return unify(arr1, arr2, theta, 0);
    }

    private static Map<Variable, Value> unify(Value[] arr1, Value[] arr2, Map<Variable, Value> theta, int start) {

        if (arr1.length != arr2.length) {
            return null;
        }

        if (arr1.length == 0) {
            return theta;
        }

        if (start == arr1.length) {

            return theta;
        }

        return unify(arr1, arr2, unify(arr1[start], arr2[start], theta), start + 1);

    }

    private static Map<Variable, Value> unifyVar(Variable var, Value x, Map<Variable, Value> theta) {

        if(theta.containsKey(var)){
            Value val = theta.get(var);
            return unify(val, x, theta);
        }
        else if(theta.containsKey(x)){
            Value val = theta.get(x);

            return unify(var, val, theta);
        }
        else if (x.occurs(var)) {
            return  null;
        }

        else {

            theta.put(var, x);
            return theta;
        }



    }
}
