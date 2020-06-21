package com.naveensundarg.shadow.prover.core.proof;

import com.naveensundarg.shadow.prover.representations.formula.*;
import com.naveensundarg.shadow.prover.representations.value.Compound;
import com.naveensundarg.shadow.prover.representations.value.Constant;
import com.naveensundarg.shadow.prover.representations.value.Value;
import com.naveensundarg.shadow.prover.representations.value.Variable;
import com.naveensundarg.shadow.prover.utils.*;

import java.util.*;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newMap;

/**
 * Created by naveensundarg on 4/11/16.
 */
public class Unifier {


    public static List<Pair<Value, Value>> getVariations(Value v1, Value v2) {

        if(v1.equals(v2)){

            return CollectionUtils.newEmptyList();
        }

        if(v1 instanceof Compound && v2 instanceof Compound) {


            if(v1.getName().equals(v2.getName()) && v1.getArguments().length == v2.getArguments().length){

                List<Pair<Value, Value>> answer = CollectionUtils.newEmptyList();


                for(int i = 0; i< v1.getArguments().length; i++){

                    answer.addAll(getVariations(v1.getArguments()[i], v2.getArguments()[i]));

                }

                answer.add(ImmutablePair.from(v1, v2));
                return answer;
            }

            return CollectionUtils.listOf(ImmutablePair.from(v1, v2));


        }



        if(v1 instanceof Variable || v2 instanceof Variable){

            return CollectionUtils.newEmptyList();
        }

        return CollectionUtils.listOf(ImmutablePair.from(v1, v2));



    }
    public static Optional<List<Pair<Value, Value>>> getVariations(Formula f1, Formula f2){

        if(f1.getLevel()>=2 || f2.getLevel()>=2){

            throw new UnsupportedOperationException("unify formula not supported for modals");
        }

        if(!f1.getClass().equals(f2.getClass())){

            return Optional.empty();

        }

        if( f1 instanceof Predicate && f2 instanceof Predicate){

            Predicate p1 = (Predicate) f1;
            Predicate p2 = (Predicate) f2;

            if(!p1.getName().equals(p2.getName())){

                return Optional.empty();
            }

            if(p1.getArguments().length!=p2.getArguments().length){
                return Optional.empty();

            }

            List<Pair<Value, Value>> variationsMap = CollectionUtils.newEmptyList();

            for(int i = 0; i < p1.getArguments().length; i++){

                if(!p1.getArguments()[i].equals(p2.getArguments()[i])){

                    variationsMap.add(ImmutablePair.from(p1.getArguments()[i], p2.getArguments()[i]));

                }

            }

            return Optional.of(variationsMap);
        }

        if( f1 instanceof Not && f2 instanceof Not){

            Not n1 = (Not) f1;
            Not n2  = (Not) f2;

            return Unifier.getVariations(n1.getArgument(), n2.getArgument());
        }


        if( f1 instanceof Implication && f2 instanceof Implication){

            Implication i1 = (Implication) f1;
            Implication i2 = (Implication) f2;

            Optional<List<Pair<Value, Value>>> map1 = Unifier.getVariations(i1.getAntecedent(), i2.getAntecedent());

            if(!map1.isPresent()){

                return Optional.empty();

            }

            Optional<List<Pair<Value, Value>>> map2 = Unifier.getVariations(i1.getConsequent(), i2.getConsequent());

            if(!map2.isPresent()){

                return Optional.empty();

            }

            List<Pair<Value, Value>> variationsMap = CollectionUtils.newEmptyList();

            variationsMap.addAll(map1.get());
            variationsMap.addAll(map2.get());

            return Optional.of(variationsMap);

        }

        if( f1 instanceof BiConditional && f2 instanceof BiConditional){

            BiConditional b1 = (BiConditional) f1;
            BiConditional b2 = (BiConditional) f2;

            Optional<List<Pair<Value, Value>>> map1 = Unifier.getVariations(b1.getLeft(), b2.getLeft());

            if(!map1.isPresent()){

                return Optional.empty();

            }

            Optional<List<Pair<Value, Value>>> map2 = Unifier.getVariations(b1.getRight(), b2.getRight());

            if(!map2.isPresent()){

                return Optional.empty();

            }

            List<Pair<Value, Value>> variationsMap = CollectionUtils.newEmptyList();

            variationsMap.addAll(map1.get());
            variationsMap.addAll(map2.get());

            return Optional.of(variationsMap);


        }

        if( f1 instanceof And && f2 instanceof And){

            And and1 = (And) f1;
            And and2 = (And) f2;

            Formula[] args1 = and1.getArguments();
            Formula[] args2 = and2.getArguments();

            if(args1.length!=args2.length){

                return Optional.empty();

            }

            List<Pair<Value, Value>> possibleAnswer = CollectionUtils.newEmptyList();

            for(int i = 0; i<args1.length; i++){

                Optional<List<Pair<Value, Value>>> mapOpt = getVariations(args1[i], args2[i]);

                if(!mapOpt.isPresent()){

                    return Optional.empty();

                }

                possibleAnswer.addAll(mapOpt.get());

            }

            return Optional.of(possibleAnswer);

        }

        if( f1 instanceof Or && f2 instanceof Or){

            Or or1 = (Or) f1;
            Or or2 = (Or) f2;

            Formula[] args1 = or1.getArguments();
            Formula[] args2 = or2.getArguments();

            if(args1.length!=args2.length){

                return Optional.empty();

            }

            List<Pair<Value, Value>> possibleAnswer = CollectionUtils.newEmptyList();

            for(int i = 0; i<args1.length; i++){

                Optional<List<Pair<Value, Value>>> mapOpt = getVariations(args1[i], args2[i]);

                if(!mapOpt.isPresent()){

                    return Optional.empty();

                }

                possibleAnswer.addAll(mapOpt.get());

            }

            return Optional.of(possibleAnswer);

        }

        if( f1 instanceof Universal && f2 instanceof Universal){

            Universal universal1 = (Universal) f1;
            Universal universal2 = (Universal) f2;


            if(!Arrays.equals(universal1.vars(), universal2.vars())){

                return Optional.empty();

            }

            Formula arg1 = universal1.getArgument();
            Formula arg2 = universal2.getArgument();

            Optional<List<Pair<Value, Value>>>  mapOpt = getVariations(arg1, arg2);

            if(!mapOpt.isPresent()){

                return  Optional.empty();

            }
            Set<Value> replaced = mapOpt.get().stream().map(Pair::first).collect(Collectors.toSet());

            Set<Variable> univVars = Arrays.stream(universal1.vars()).collect(Collectors.toSet());

            if(replaced.stream().anyMatch(univVars::contains)){

                return Optional.empty();
            } else {

                return mapOpt;
            }


        }

        if( f1 instanceof Existential && f2 instanceof Existential){

            Existential existential1 = (Existential) f1;
            Existential existential2 = (Existential) f2;


            if(!Arrays.equals(existential1.vars(), existential2.vars())){

                return Optional.empty();

            }

            Formula arg1 = existential1.getArgument();
            Formula arg2 = existential2.getArgument();

            Optional<List<Pair<Value, Value>>>  mapOpt = getVariations(arg1, arg2);

            if(!mapOpt.isPresent()){

                return  Optional.empty();

            }
            Set<Value> replaced = mapOpt.get().stream().map(Pair::first).collect(Collectors.toSet());

            Set<Variable> univVars = Arrays.stream(existential1.vars()).collect(Collectors.toSet());

            if(replaced.stream().anyMatch(univVars::contains)){

                return Optional.empty();
            } else {

                return mapOpt;
            }

        }



        return Optional.empty();

    }

    public static boolean fullFormulaIsVariable(Formula formula) {

        if(formula instanceof Predicate){

           return isVariable(((Predicate) formula).getName()) && ((Predicate) formula).getArguments().length == 0;
        }
        return false;
    }

    public static Optional<Map<Variable, Value>> unifyFormula(Formula f1, Formula f2){

        if(fullFormulaIsVariable(f1)){

            try {
                Variable variable = new Variable(((Predicate)f1).getName());
                Value value =  Reader.readLogicValueFromString(f2.toString());
                return Optional.of(CollectionUtils.mapWith(variable, value));
            } catch (Reader.ParsingException e) {
                e.printStackTrace();
            }
        }
        if(fullFormulaIsVariable(f2)){

            try {
                Variable variable = new Variable(((Predicate)f2).getName());
                Value value =  Reader.readLogicValueFromString(f1.toString());
                return Optional.of(CollectionUtils.mapWith(variable, value));
            } catch (Reader.ParsingException e) {
                e.printStackTrace();
            }
        }

        if(! (f1.getClass().isAssignableFrom(f2.getClass()) || f2.getClass().isAssignableFrom(f1.getClass())) ){

            return Optional.empty();

        }

        if( f1 instanceof Predicate && f2 instanceof Predicate){

           Map<Variable, Value> map = Unifier.unify((Predicate) f1, (Predicate) f2);

           return map==null? Optional.empty() : Optional.of(map);
        }

        if( f1 instanceof Not && f2 instanceof Not){

            Not n1 = (Not) f1;
            Not n2  = (Not) f2;

            return Unifier.unifyFormula(n1.getArgument(), n2.getArgument());
        }


        if( f1 instanceof Implication && f2 instanceof Implication){

            Implication i1 = (Implication) f1;
            Implication i2 = (Implication) f2;

            Optional<Map<Variable, Value>> map1 = Unifier.unifyFormula(i1.getAntecedent(), i2.getAntecedent());

            if(!map1.isPresent()){

                return Optional.empty();

            }

            Optional<Map<Variable, Value>> map2 = Unifier.unifyFormula(i1.getConsequent(), i2.getConsequent());

            if(!map2.isPresent()){

                return Optional.empty();

            }

            return Unifier.addTo(map1.get(), map2.get());

        }

        if( f1 instanceof BiConditional && f2 instanceof BiConditional){

            BiConditional b1 = (BiConditional) f1;
            BiConditional b2 = (BiConditional) f2;

            Optional<Map<Variable, Value>> map1 = Unifier.unifyFormula(b1.getLeft(), b2.getLeft());

            if(!map1.isPresent()){

                return Optional.empty();

            }

            Optional<Map<Variable, Value>> map2 = Unifier.unifyFormula(b1.getRight(), b2.getRight());

            if(!map2.isPresent()){

                return Optional.empty();

            }

            return Unifier.addTo(map1.get(), map2.get());

        }

        if( f1 instanceof And && f2 instanceof And){

            And and1 = (And) f1;
            And and2 = (And) f2;

            Formula[] args1 = and1.getArguments();
            Formula[] args2 = and2.getArguments();

            if(args1.length!=args2.length){

                return Optional.empty();

            }

            Map<Variable, Value> possibleAnswer = CollectionUtils.newMap();

            for(int i = 0; i<args1.length; i++){

                Optional<Map<Variable, Value>> mapOpt = unifyFormula(args1[i], args2[i]);

                if(!mapOpt.isPresent()){

                    return Optional.empty();

                }

                Optional<Map<Variable, Value>> augmentedOpt = Unifier.addTo(possibleAnswer, mapOpt.get());

                if(!augmentedOpt.isPresent()){

                    return Optional.empty();

                } else {

                    possibleAnswer  = augmentedOpt.get();

                }


            }

            return Optional.of(possibleAnswer);

        }

        if( f1 instanceof Or && f2 instanceof Or){

            Or or1 = (Or) f1;
            Or or2 = (Or) f2;

            Formula[] args1 = or1.getArguments();
            Formula[] args2 = or2.getArguments();

            if(args1.length!=args2.length){
                return Optional.empty();
            }

            Map<Variable, Value> possibleAnswer = CollectionUtils.newMap();

            for(int i = 0; i<args1.length; i++){

                Optional<Map<Variable, Value>> mapOpt = unifyFormula(args1[i], args2[i]);

                if(!mapOpt.isPresent()){

                    return Optional.empty();

                }

                Optional<Map<Variable, Value>> augmentedOpt = Unifier.addTo(possibleAnswer, mapOpt.get());

                if(!augmentedOpt.isPresent()){

                    return Optional.empty();

                } else {

                    possibleAnswer  = augmentedOpt.get();

                }

            }


            return Optional.of(possibleAnswer);

        }

        if( f1 instanceof Universal && f2 instanceof Universal){

            Universal universal1 = (Universal) f1;
            Universal universal2 = (Universal) f2;


            if(!Arrays.equals(universal1.vars(), universal2.vars())){

                return Optional.empty();

            }

            Formula arg1 = universal1.getArgument();
            Formula arg2 = universal2.getArgument();

            Optional<Map<Variable, Value>> mapOpt = Unifier.unifyFormula(arg1, arg2);

            if(!mapOpt.isPresent()){

                return  Optional.empty();

            }
            Set<Variable> boundVars = mapOpt.get().keySet();

            Set<Variable> univVars = Arrays.stream(universal1.vars()).collect(Collectors.toSet());
            if(boundVars.stream().anyMatch(univVars::contains)){

                return Optional.empty();
            } else {

                return mapOpt;
            }


        }

        if( f1 instanceof Existential && f2 instanceof Existential){

            Existential existential1 = (Existential) f1;
            Existential existential2 = (Existential) f2;


            if(!Arrays.equals(existential1.vars(), existential2.vars())){

                return Optional.empty();

            }

            Formula arg1 = existential1.getArgument();
            Formula arg2 = existential2.getArgument();

            Optional<Map<Variable, Value>> mapOpt = Unifier.unifyFormula(arg1, arg2);

            if(!mapOpt.isPresent()){

                return  Optional.empty();

            }
            Set<Variable> boundVars = mapOpt.get().keySet();

            Set<Variable> existVars = Arrays.stream(existential1.vars()).collect(Collectors.toSet());
            if(boundVars.stream().anyMatch(existVars::contains)){

                return Optional.empty();
            } else {

                return mapOpt;
            }

        }

        return Optional.empty();

    }

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

            Formula f1 = k1.getFormula();
            Formula f2 = k2.getFormula();

            Map<Variable, Value> m1 = unify(t1, t2);
            Map<Variable, Value> m2 = unify(a1, a2);
            Map<Variable, Value> m3 = combineVariableValueMap(m1, m2);
            Optional<Map<Variable, Value>> m4Opt= unifyFormula(f1, f2);

            if(m4Opt.isPresent()) {
                return combineVariableValueMap(m3, m4Opt.get());

            } else {
                return m3;
            }


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

    public static boolean isVariable(String name){

        return name.endsWith("?");
    }
    public static Map<Variable, Value> unify(Predicate p1, Predicate p2) {

        boolean isVariable = false;

        if (!p1.getName().equals(p2.getName()) && !isVariable(p1.getName()) && !isVariable(p2.getName())) {
            return null;
        }
        Map<Variable, Value> theta = newMap();
        if(isVariable(p1.getName())){
            isVariable = true;
            if(p1.getArguments().length == 0) {

                try {
                    theta.put(new Variable(p1.getName()), Reader.readLogicValueFromString(p2.toString()));
                } catch (Reader.ParsingException e) {
                    e.printStackTrace();
                }
                return theta;

            } else {
                theta.put(new Variable(p1.getName()), new Constant(p2.getName()));

            }
        }

        if(isVariable(p2.getName())){

            if(p2.getArguments().length == 0) {

                try {
                    theta.put(new Variable(p2.getName()), Reader.readLogicValueFromString(p1.toString()));
                } catch (Reader.ParsingException e) {
                    e.printStackTrace();
                }
                return theta;

            } else {
                theta.put(new Variable(p2.getName()), new Constant(p1.getName()));

            }
        }

        if (!isVariable &&  p1.getArguments().length != p2.getArguments().length) {
            return null;
        }

        if(isVariable && (p1.getArguments().length == 0 || p2.getArguments().length == 0)) {

            return theta;
        }


        Value[] args1 = p1.getArguments();
        Value[] args2 = p2.getArguments();

        return unify(args1, args2, theta, 0);

    }


    public static Map<Variable, Value> unify(Value v1, Value v2) {


        return unify(v1, v2, newMap());

    }


    protected static Map<Variable, Value> unify(Value x, Value y, Map<Variable, Value> theta) {

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

    protected static Map<Variable, Value> unify(Value[] arr1, Value[] arr2, Map<Variable, Value> theta) {

        return unify(arr1, arr2, theta, 0);
    }

    protected static Map<Variable, Value> unify(Value[] arr1, Value[] arr2, Map<Variable, Value> theta, int start) {

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
            return  null;//Arrays.stream(Z.getArguments()).map(zArg -> subUnify(x, zArg)).reduce(Sets.newSet(), Sets::union);
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
