package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.Expression;
import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.CollectionUtils;
import com.naveensundarg.shadow.prover.utils.Pair;
import com.naveensundarg.shadow.prover.utils.Sets;

import java.util.*;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Unifier {


    public static Map<Variable, Value> unify(BaseFormula bf1, BaseFormula bf2) {

        if (!bf1.getClass().equals(bf2.getClass())) {
            return null;
        }

        if (bf1 instanceof Predicate) {

            Predicate p1 = (Predicate) bf1;
            Predicate p2 = (Predicate) bf2;


            return unify(p1, p2);
        }

        if (bf1 instanceof Common) {

            Common c1 = (Common) bf1;
            Common c2 = (Common) bf2;

            Value t1 = c1.getTime();
            Value t2 = c2.getTime();


            Map<Variable, Value> s1 = unify(t1, t2);

            return s1;


        }

        if (bf1 instanceof Knowledge) {

            Knowledge k1 = (Knowledge) bf1;
            Knowledge k2 = (Knowledge) bf2;


            Value t1 = k1.getTime();
            Value t2 = k2.getTime();

            Value a1 = k1.getAgent();
            Value a2 = k2.getAgent();


            Map<Variable, Value> m1 = unify(t1, t2);
            Map<Variable, Value> m2 = unify(a1, a2);

            return combineVariableValueMap(m1, m2);


        }

        if (bf1 instanceof Belief) {

            Belief b1 = (Belief) bf1;
            Belief b2 = (Belief) bf2;


            Value t1 = b1.getTime();
            Value t2 = b2.getTime();

            Value a1 = b1.getAgent();
            Value a2 = b2.getAgent();


            if (!b1.getFormula().getClass().equals(b2.getFormula().getClass())) {
                return CollectionUtils.newMap();
            }
            Map<Variable, Value> m1 = unify(t1, t2);
            Map<Variable, Value> m2 = unify(a1, a2);

            return combineVariableValueMap(m1, m2);


        }

        if (bf1 instanceof Perception) {

            Perception p1 = (Perception) bf1;
            Perception p2 = (Perception) bf2;


            Value t1 = p1.getTime();
            Value t2 = p2.getTime();

            Value a1 = p1.getAgent();
            Value a2 = p2.getAgent();


            Map<Variable, Value> m1 = unify(t1, t2);
            Map<Variable, Value> m2 = unify(a1, a2);


            return combineVariableValueMap(m1, m2);


        }

        if (bf1 instanceof Ought) {

            Ought o1 = (Ought) bf1;
            Ought o2 = (Ought) bf2;


            Value t1 = o1.getTime();
            Value t2 = o2.getTime();

            Value a1 = o1.getAgent();
            Value a2 = o2.getAgent();


            Map<Variable, Value> m1 = unify(t1, t2);
            Map<Variable, Value> m2 = unify(a1, a2);

            return combineVariableValueMap(m1, m2);


        }

        if (bf1 instanceof Says) {

            Says s1 = (Says) bf1;
            Says s2 = (Says) bf2;


            Value t1 = s1.getTime();
            Value t2 = s2.getTime();

            Value a1 = s1.getAgent();
            Value a2 = s2.getAgent();


            Map<Variable, Value> m1 = unify(t1, t2);
            Map<Variable, Value> m2 = unify(a1, a2);

            return combineVariableValueMap(m1, m2);


        }

        if (bf1 instanceof Desire) {

            Desire s1 = (Desire) bf1;
            Desire s2 = (Desire) bf2;


            Value t1 = s1.getTime();
            Value t2 = s2.getTime();

            Value a1 = s1.getAgent();
            Value a2 = s2.getAgent();


            Map<Variable, Value> m1 = unify(t1, t2);
            Map<Variable, Value> m2 = unify(a1, a2);

            return combineVariableValueMap(m1, m2);


        }

        throw new AssertionError("Unaccounted base formula encountered during unification: " + bf1 + " and " + bf2);

    }

    private static Map<Variable, Value> combineVariableValueMap(Map<Variable, Value> s1, Map<Variable, Value> s2) {
        if (s1 == null) {
            s1 = CollectionUtils.newMap();
        }
        if (s2 == null) {
            s2 = CollectionUtils.newMap();
        }

        Map<Variable, Value> all = CollectionUtils.newMap();

        all.putAll(s1);
        all.putAll(s2);

        return all;
    }

    public static Map<Variable, Value> unify(Predicate p1, Predicate p2) {


        if (!p1.getName().equals(p2.getName())) {
            return null;
        }

        if (p1.getArguments().length != p2.getArguments().length) {
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

        } else if (x.isCompound() && y.isCompound()) {

            if (x.getName().equals(y.getName())) {

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


        if (theta.containsKey(var)) {
            Value val = theta.get(var);
            return unify(val, x, theta);
        } else if (theta.containsKey(x)) {
            Value val = theta.get(x);

            return unify(var, val, theta);
        }

        Value xx = x;
        if (theta.values().stream().anyMatch(b -> b.occurs(var))) {
            xx = x.apply(theta);
        }

         if (xx.occurs(var)) {
            return  null;
        }

        else {

            theta.put(var, x);
            return theta;
        }
    }

    public static Set<Map<Variable, Value>> subUnify(Value x, Value Z) {

        Map<Variable, Value> theta = unify(x, Z);
        if (theta != null) {
            return Sets.with(theta);
        } else {
            return Arrays.stream(Z.getArguments()).map(zArg -> subUnify(x, zArg)).reduce(Sets.newSet(), Sets::union);
        }

    }

    public static Optional<Map<Variable, Value>> addTo(Map<Variable, Value> existingBindings, Variable variable, Value value) {


        Map<Variable, Value> newBindings = CollectionUtils.newMapFrom(existingBindings);

        if (!existingBindings.containsKey(variable)) {

            newBindings.put(variable, value);

            return Optional.of(newBindings);
        } else {

            Value existingValue = existingBindings.get(variable);

            Set<Map<Variable, Value>> bindings = Unifier.subUnify(value, existingValue);

            if (bindings == null || bindings.stream().anyMatch(Objects::isNull)) {
                return Optional.empty();

            } else {

                for (Map<Variable, Value> binding : bindings) {

                    newBindings.putAll(binding);
                }
            }

            return Optional.of(newBindings);
        }

    }

    public static Optional<Map<Variable, Value>> addTo(Map<Variable, Value> existingBindings, Map<Variable, Value> delta) {


        Map<Variable, Value> newBindings = CollectionUtils.newMapFrom(existingBindings);


        for (Map.Entry<Variable, Value> entry : delta.entrySet()) {

            Variable variable = entry.getKey();
            Value value = entry.getValue();

            Optional<Map<Variable, Value>> thisResult = addTo(newBindings, variable, value);

            if (!thisResult.isPresent()) {

                return thisResult;

            } else {

                newBindings.putAll(thisResult.get());

            }

        }

        return Optional.of(newBindings);

    }

}
