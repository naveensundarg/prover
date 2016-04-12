package com.naveensundarg.shadow.prover.utils;

import java.util.ArrayList;
import java.util.List;

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

    public static <T> List<T> add(List<T> list, T a){
        List<T> newList = newList();
        newList.addAll(list);
        newList.add(a);
        return newList;
    }
}
