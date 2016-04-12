package com.naveensundarg.shadow.prover.utils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static com.naveensundarg.shadow.prover.utils.CollectionUtils.add;
import static com.naveensundarg.shadow.prover.utils.CollectionUtils.listOf;
import static com.naveensundarg.shadow.prover.utils.CollectionUtils.newList;

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

    public static <T> Set<T> difference(Set<T> X, Set<T> Y){

        Set<T> newSet = copy(X);

        newSet.removeAll(Y);

        return newSet;
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


    public static void main(String[] args){

        Set<String> set1 = newSet();
        Set<String> set2 = newSet();
        Set<String> set3 = newSet();

        set1.add("a");
        set1.add("b");

        set2.add("x");
        set2.add("y");
        set2.add("z");

        List<Set<String>> sets = newList();

        sets.add(set2);
      sets.add(set1);

        System.out.println(cartesianProduct(sets));

    }
}
