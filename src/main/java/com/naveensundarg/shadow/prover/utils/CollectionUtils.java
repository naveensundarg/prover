package com.naveensundarg.shadow.prover.utils;

import java.util.*;

/**
 * Created by naveensundarg on 4/10/16.
 */
public class CollectionUtils {

    public static <T> List<T> newList(){
        return new ArrayList<>();
    }

    public static <T> List<T> listOf(T a){
        List<T> list = newList();
        list.add(a);
        return list;
    }


    public static <U,V> Map<U,V> newMap(){
        return new HashMap<>();
    }

    public static <T> List<T> add(List<T> list, T a){
        List<T> newList = newList();
        newList.addAll(list);
        newList.add(a);
        return newList;
    }

    public static <T> Set<T> setFrom(Set<T> set){
        return new HashSet<>(set);
    }
}
