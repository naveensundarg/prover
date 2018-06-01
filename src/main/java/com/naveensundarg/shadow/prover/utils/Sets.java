package com.naveensundarg.shadow.prover.utils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newEmptyList;

/**
 * Created by naveensundarg on 4/8/16.
 */
public class Sets {

    public static <T> Set<T> newSet(){
        return new HashSet<>();
    }

    public static <T> Set<T> copy(Set<T> set){
        return new HashSet<>(set);
    }
    public static <T> Set<T> add(Set<T> set, T t){

        Set<T> newSet = copy(set);

        newSet.add(t);

        return newSet;
    }

    public static <T> Set<T> with(T t){

        Set<T> newSet = newSet();

        newSet.add(t);

        return newSet;
    }

    public static <T> Set<T> fromArray(T[] t){

        Set<T> newSet = newSet();

        newSet.addAll(Arrays.asList(t));

        return newSet;
    }

    public static <T> Set<T> from(T... t){

        Set<T> newSet = newSet();

        newSet.addAll(Arrays.asList(t));

        return newSet;
    }

    public static <T> Set<T> remove(Set<T> set, T t){

        Set<T> newSet = copy(set);

        newSet.remove(t);

        return newSet;
    }

    public static <T> Set<T> union(Set<T> X, Set<T> Y){

        Set<T> newSet = copy(X);

        newSet.addAll(Y);

        return newSet;
    }

    public static <T> Set<T> unionOf(Set<T> ... Xs ){

        Set<T> newSet = newSet();

        for(Set<T> x:Xs){

            newSet.addAll(x);

        }

        return newSet;
    }

    public static <T> Set<T> difference(Set<T> X, Set<T> Y){

        Set<T> newSet = copy(X);

        newSet.removeAll(Y);

        return newSet;
    }



    public static <T> Set<T> intersection(Set<T> X, Set<T> Y){

        Set<T> newSet = newSet();

        X.stream().forEach(x-> {
            if(Y.contains(x)){
                newSet.add(x);
            }
        });

        return newSet;
    }

    public static <T> Set<List<T>> binaryProduct(Set<T> set){

        List<Set<T>> sets = newEmptyList();

        sets.add(set);
        sets.add(set);
        return cartesianProduct(sets);
    }

    public static <T> boolean subset(Set<T> P, Set<T> Q){

        return difference(P,Q).size()==0;
    }

    public static <T> Set<List<T>> cartesianProduct(List<Set<T>> sets){


        if(sets.size() == 0){
            return newSet();
        }

        if(sets.size() == 1){

            Set<List<T>> answer = newSet();

            return sets.get(0).stream().map(CollectionUtils::listOf).collect(Collectors.toSet());
        }

        else {

            Set<List<T>> smaller = cartesianProduct(sets.subList(1, sets.size()));

             return  sets.get(0).stream().
                     map(x-> smaller.stream().
                             map(y-> CollectionUtils.add(y, x)).
                     collect(Collectors.toSet())).
                     reduce(newSet(), Sets::union);

        }
    }



}
