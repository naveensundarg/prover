package com.naveensundarg.shadow.prover.utils;

import java.util.*;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class CollectionUtils {

    public static <T> List<T> newEmptyList(){
        return new ArrayList<>();
    }

    public static <T> Set<T> newEmptySet(){
        return new HashSet<>();
    }

    public static <T> List<T> listOf(T a){
        List<T> list = newEmptyList();
        list.add(a);
        return list;
    }

    public static <T> List<T> addToList(List<T> list, T a){
        List<T> newlist = newEmptyList();
        newlist.addAll(list);
        newlist.add(a);
        return newlist;
    }


    public static <U,V> Map<U,V> newMap(){
        return new HashMap<>();
    }

    public static <T> List<T> add(List<T> list, T a){
        List<T> newList = newEmptyList();
        newList.addAll(list);
        newList.add(a);
        return newList;
    }

    public static <T> Set<T> union(Set<T> x, Set<T> y){

        Set<T> result = newEmptySet();

        result.addAll(x);
        result.addAll(y);
        return result;
    }

    public static <T> Set<T> setFrom(Set<T> set){
        return new HashSet<>(set);
    }
}
